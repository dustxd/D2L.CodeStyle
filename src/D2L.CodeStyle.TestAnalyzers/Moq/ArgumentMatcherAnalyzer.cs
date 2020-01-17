using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using D2L.CodeStyle.TestAnalyzers.Common;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.TestAnalyzers.Moq {

	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	internal sealed class ArgumentMatcherAnalyzer : DiagnosticAnalyzer {

		private const string MoqItFullName = "Moq.It";

		private static readonly ImmutableArray<string> ItArgumentMatcherMethods = ImmutableArray.Create(
				"Is",
				"IsAny",
				"IsIn",
				"IsInRange",
				"IsNotIn",
				"IsNotNull"
			);

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(
			Diagnostics.TestMoqArgumentMatcherTypeMismatch
		);

		public override void Initialize( AnalysisContext context ) {

			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterAnalysis );
		}

		private void RegisterAnalysis( CompilationStartAnalysisContext context ) {

			Compilation compilation = context.Compilation;

			INamedTypeSymbol itType = compilation.GetTypeByMetadataName( MoqItFullName );
			if( itType == null ) {
				return;
			}

			IImmutableSet<ISymbol> argumentMatchers = GetItArgumentMatcherMethods( itType, ItArgumentMatcherMethods );

			context.RegisterSyntaxNodeAction(
					ctxt => {
						if( ctxt.Node is InvocationExpressionSyntax invocation ) {
							AnalyzeMethodInvocation( ctxt, invocation, argumentMatchers );
						}
					},
					SyntaxKind.InvocationExpression
				);
		}

		private void AnalyzeMethodInvocation(
				SyntaxNodeAnalysisContext context,
				InvocationExpressionSyntax invocation,
				IImmutableSet<ISymbol> argumentMatchers
			) {

			ISymbol invocationSymbol = context.SemanticModel
				.GetSymbolInfo( invocation.Expression )
				.Symbol;

			if( invocationSymbol is IMethodSymbol methodSymbol ) {

				if( argumentMatchers.Contains( methodSymbol.OriginalDefinition ) ) {

					ITypeSymbol returnType = methodSymbol.ReturnType;

					if( TryGetMatchingArgumentType( context, invocation, out ITypeSymbol argumentType ) ) {

						if( !returnType.Equals( argumentType )) {

							ReportDiagnostic(
									context,
									invocation,
									argumentType,
									Diagnostics.TestMoqArgumentMatcherTypeMismatch
								);
						}
					}
				}
			}
		}

		private static bool TryGetMatchingArgumentType(
				SyntaxNodeAnalysisContext context,
				InvocationExpressionSyntax invocation,
				out ITypeSymbol argumentType
			) {

			if( invocation.Parent is ArgumentSyntax argument ) {

				if( argument.Parent is ArgumentListSyntax argumentList ) {

					int argumentIndex = argumentList.Arguments.IndexOf( argument );

					if( argumentList.Parent is InvocationExpressionSyntax caller ) {

						ISymbol callerSymbol = context.SemanticModel
							.GetSymbolInfo( caller )
							.Symbol;

						if( callerSymbol is IMethodSymbol callerMethodSymbol ) {

							argumentType = callerMethodSymbol.Parameters[argumentIndex].Type;
							return true;
						}
					}
				}
			}

			argumentType = null;
			return false;
		}

		private void ReportDiagnostic(
				SyntaxNodeAnalysisContext context,
				InvocationExpressionSyntax invocation,
				ISymbol argumentType,
				DiagnosticDescriptor diagnosticDescriptor
			) {

			Location location = invocation.GetLocation();
			string matcher = invocation.ToString();
			string argument = argumentType.ToDisplayString( MemberDisplayFormat );

			var diagnostic = Diagnostic.Create(
					diagnosticDescriptor,
					location,
					matcher,
					argument
				);

			context.ReportDiagnostic( diagnostic );
		}

		private static readonly SymbolDisplayFormat MemberDisplayFormat = new SymbolDisplayFormat(
				memberOptions: SymbolDisplayMemberOptions.IncludeContainingType,
				localOptions: SymbolDisplayLocalOptions.IncludeType,
				typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces
			);

		private static IImmutableSet<ISymbol> GetItArgumentMatcherMethods(
				INamedTypeSymbol type,
				IEnumerable<string> methodNames
			) {

			ImmutableHashSet<ISymbol>.Builder builder = ImmutableHashSet.CreateBuilder<ISymbol>();

			foreach( string name in methodNames ) {

				IEnumerable<ISymbol> methods = type
					.GetMembers( name )
					.Where( m => m.Kind == SymbolKind.Method );

				foreach( ISymbol method in methods ) {
					builder.Add( method );
				}
			}

			return builder.ToImmutableHashSet();
		}
	}
}
