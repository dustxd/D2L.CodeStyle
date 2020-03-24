using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using D2L.CodeStyle.Analyzers.Immutability;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.Analyzers.ApiUsage.Caching {

	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	internal sealed class ImmutableCacheItemAnalyzer : DiagnosticAnalyzer {

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(
			Diagnostics.CacheItemTypeShouldBeImmutable
		);

		public override void Initialize( AnalysisContext context ) {

			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterAnalysis );
		}

		private void RegisterAnalysis( CompilationStartAnalysisContext context ) {

			RegisterDangerousMethodAnalysis( context );
		}

		private void RegisterDangerousMethodAnalysis( CompilationStartAnalysisContext context ) {

			Compilation compilation = context.Compilation;

			IImmutableSet<ISymbol> genericCacheMethods = GetGenericCacheMethods( compilation );
			if( genericCacheMethods.Count == 0 ) {
				return;
			}

			MutabilityInspector inspector = new MutabilityInspector( compilation );

			context.RegisterSyntaxNodeAction(
					ctxt => {
						if( ctxt.Node is InvocationExpressionSyntax invocation ) {
							AnalyzeMethodInvocation( ctxt, invocation, genericCacheMethods, inspector );
						}
					},
					SyntaxKind.InvocationExpression
				);
		}

		private void AnalyzeMethodInvocation(
				SyntaxNodeAnalysisContext context,
				InvocationExpressionSyntax invocation,
				IImmutableSet<ISymbol> genericCacheMethods,
				MutabilityInspector inspector
			) {

			ISymbol symbol = context.SemanticModel
				.GetSymbolInfo( invocation.Expression )
				.Symbol;

			if( symbol == null ) {
				return;
			}

			if( !IsGenericCacheMethod( symbol, genericCacheMethods ) ) {
				return;
			}

			if( !( symbol is IMethodSymbol methodSymbol ) ) {
				return;
			}

			ITypeSymbol itemType = methodSymbol.TypeArguments[ methodSymbol.TypeArguments.Length - 1 ];

			if( itemType.TypeKind == TypeKind.TypeParameter ) {
				return;
			}

			MutabilityInspectionResult result = inspector.InspectConcreteType( itemType );
			if( result.IsMutable ) {

				ReportDiagnostic(
						context,
						invocation.GetLocation(),
						itemType,
						Diagnostics.CacheItemTypeShouldBeImmutable
					);
			}
		}

		private static bool IsGenericCacheMethod(
				ISymbol memberSymbol,
				IImmutableSet<ISymbol> genericCacheMethods
			) {

			ISymbol originalDefinition = memberSymbol.OriginalDefinition;

			if( genericCacheMethods.Contains( originalDefinition ) ) {
				return true;
			}

			if( memberSymbol != originalDefinition ) {

				if( genericCacheMethods.Contains( memberSymbol ) ) {
					return true;
				}
			}

			return false;
		}

		private void ReportDiagnostic(
				SyntaxNodeAnalysisContext context,
				Location location,
				ISymbol itemSymbol,
				DiagnosticDescriptor diagnosticDescriptor
			) {

			string itemName = itemSymbol.ToDisplayString( MemberDisplayFormat );

			var diagnostic = Diagnostic.Create(
					diagnosticDescriptor,
					location,
					itemName
				);

			context.ReportDiagnostic( diagnostic );
		}

		private static readonly SymbolDisplayFormat MemberDisplayFormat = new SymbolDisplayFormat(
				memberOptions: SymbolDisplayMemberOptions.IncludeContainingType,
				localOptions: SymbolDisplayLocalOptions.IncludeType,
				typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces
			);

		private static IImmutableSet<ISymbol> GetGenericCacheMethods( Compilation compilation ) {

			ImmutableHashSet<ISymbol>.Builder builder = ImmutableHashSet.CreateBuilder<ISymbol>();

			foreach( KeyValuePair<string, ImmutableArray<string>> pairs in CacheMethods.Definitions ) {

				INamedTypeSymbol type = compilation.GetTypeByMetadataName( pairs.Key );
				if( type != null ) {

					foreach( string name in pairs.Value ) {

						IEnumerable<ISymbol> methods = type
							.GetMembers( name )
							.Where( m => m.Kind == SymbolKind.Method );

						foreach( ISymbol method in methods ) {
							builder.Add( method );
						}
					}
				}
			}

			return builder.ToImmutableHashSet();
		}
	}
}
