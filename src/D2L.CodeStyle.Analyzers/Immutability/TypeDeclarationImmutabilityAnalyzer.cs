using System;
using System.Collections.Immutable;
using System.Linq;
using D2L.CodeStyle.Analyzers.Extensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.Analyzers.Immutability {
	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	public sealed class TypeDeclarationImmutabilityAnalyzer : DiagnosticAnalyzer {

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(
			Diagnostics.MemberIsNotReadOnly,
			Diagnostics.NonImmutableTypeHeldByImmutable
		);

		public override void Initialize( AnalysisContext context ) {
			// context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( CompilationStart );
		}

		public static void CompilationStart(
			CompilationStartAnalysisContext context
		) {
			ImmutabilityContext immutabilityContext = new ImmutabilityContext( Array.Empty<ImmutableTypeInfo>() );

			context.RegisterSymbolAction(
				ctx => AnalyzeNamedType(
					ctx,
					immutabilityContext,
					(INamedTypeSymbol)ctx.Symbol
				),
				SymbolKind.NamedType
			);
			
		}

		private static void AnalyzeNamedType(
			SymbolAnalysisContext context,
			ImmutabilityContext immutabilityContext,
			INamedTypeSymbol analyzedType
		) {
			if( !ShouldAnalyze( analyzedType ) ) {
				return;
			}

			ImmutableArray<ISymbol> membersToAnalyze = analyzedType.GetMembers().Where( s => !s.IsStatic ).ToImmutableArray();
			foreach( ISymbol member in membersToAnalyze ) {
				switch( member.Kind ) {
					case SymbolKind.Method:
						// Methods aren't data and don't contribute to mutability
						continue;
					case SymbolKind.NamedType:
						// Type declarations aren't data and don't contribute to mutability
						continue;
					case SymbolKind.Property:
						// Auto-implemented properties matter, but are taken care of via the backing field
						continue;
					case SymbolKind.Field:
						AnalyzeField(
							context,
							immutabilityContext,
							analyzedType,
							member as IFieldSymbol
						);
						break;
					default:
						continue;
				}
			}
		}

		private static void AnalyzeField(
			SymbolAnalysisContext context,
			ImmutabilityContext immutabilityContext,
			INamedTypeSymbol analyzedType,
			IFieldSymbol field
		) {
			if( field.IsConst ) {
				return;
			}

			ISymbol visibleSymbol = field.AssociatedSymbol ?? field;
			if( IsAudited( visibleSymbol ) ) {
				return;
			}

			if( !field.IsReadOnly ) {
				context.ReportDiagnostic(
					Diagnostic.Create(
						Diagnostics.MemberIsNotReadOnly,
						location: visibleSymbol.Locations.First(),
						messageArgs: new object[] { visibleSymbol.Kind, visibleSymbol.Name, field.ContainingType.Name }
					)
				);
				return;
			}

			ITypeSymbol initializerType = null;
			ExpressionSyntax initializer = visibleSymbol.Equals( field )
				? visibleSymbol.GetDeclarationSyntax<VariableDeclaratorSyntax>().Initializer?.Value
				: visibleSymbol.GetDeclarationSyntax<PropertyDeclarationSyntax>().Initializer?.Value;
			if( initializer != null ) {
				SemanticModel model = context.Compilation.GetSemanticModel( initializer.SyntaxTree );
				TypeInfo initializerTypeInfo = model.GetTypeInfo( initializer );
				initializerType = initializerTypeInfo.Type;
			}

			if( !immutabilityContext.IsImmutable(
				initializerType ?? field.Type,
				field.Type.Equals( initializerType ) ? ImmutableTypeKind.Total : ImmutableTypeKind.Instance,
				visibleSymbol.Locations.First(),
				out Diagnostic heldTypeDiagnostic
			) ) {
				context.ReportDiagnostic( heldTypeDiagnostic );
				return;
			}
		}

		private static bool IsAudited( ISymbol symbol ) {
			if( Attributes.Mutability.Audited.IsDefined( symbol ) ) {
				return true;
			}

			if( Attributes.Mutability.Unaudited.IsDefined( symbol ) ) {
				return true;
			}

			return false;
		}

		private static bool ShouldAnalyze(
			INamedTypeSymbol analyzedType
		) {
			if( analyzedType.TypeKind == TypeKind.Interface ) {
				return false;
			}

			if( Attributes.Objects.Immutable.IsDefined( analyzedType ) ) {
				return true;
			}

			if( Attributes.Objects.ImmutableBaseClass.IsDefined( analyzedType ) ) {
				return true;
			}

			return false;
		}
	}
}
