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

			ISymbol methodSymbol = context.SemanticModel
				.GetSymbolInfo( invocation.Expression )
				.Symbol;

			if( !AnalyzePotentiallyDangerousMember( context, methodSymbol, auditedAttributeType, unauditedAttributeType, dangerousMethods ) ) {
				return;
			}

			ReportDiagnostic( context, methodSymbol, Diagnostics.DangerousMethodsShouldBeAvoided );
		}

		private void AnalyzePropertyAccess(
				SyntaxNodeAnalysisContext context,
				MemberAccessExpressionSyntax propertyAccess,
				INamedTypeSymbol auditedAttributeType,
				INamedTypeSymbol unauditedAttributeType,
				IImmutableSet<ISymbol> dangerousProperties
			) {

			ISymbol propertySymbol = context.SemanticModel
				.GetSymbolInfo( propertyAccess )
				.Symbol;

			if( !AnalyzePotentiallyDangerousMember( context, propertySymbol, auditedAttributeType, unauditedAttributeType, dangerousProperties ) ) {
				return;
			}

			ReportDiagnostic( context, propertySymbol, Diagnostics.DangerousPropertiesShouldBeAvoided );
		}

		private bool AnalyzePotentiallyDangerousMember(
				SyntaxNodeAnalysisContext context,
				ISymbol memberSymbol,
				INamedTypeSymbol auditedAttributeType,
				INamedTypeSymbol unauditedAttributeType,
				IImmutableSet<ISymbol> dangerousMembers
			) {

			if( memberSymbol.IsNullOrErrorType() ) {
				return false;
			}

			if( !IsDangerousMemberSymbol( memberSymbol, dangerousMembers ) ) {
				return false;
			}

			bool isAudited = context.ContainingSymbol
				.GetAttributes()
				.Any( attr => IsAuditedAttribute( auditedAttributeType, unauditedAttributeType, attr, memberSymbol ) );

			if( isAudited ) {
				return false;
			}

			return true;
		}

		private static bool IsDangerousMemberSymbol(
				ISymbol memberSymbol,
				IImmutableSet<ISymbol> dangerousMembers
			) {

			ISymbol originalDefinition = memberSymbol.OriginalDefinition;

			if( dangerousMembers.Contains( originalDefinition ) ) {
				return true;
			}

			if( memberSymbol != originalDefinition ) {

				if( dangerousMembers.Contains( memberSymbol ) ) {
					return true;
				}
			}

			return false;
		}

		private static bool IsAuditedAttribute(
				INamedTypeSymbol auditedAttributeType,
				INamedTypeSymbol unauditedAttributeType,
				AttributeData attr,
				ISymbol memberSymbol
			) {

			bool isAudited = (
					attr.AttributeClass.Equals( auditedAttributeType )
					|| attr.AttributeClass.Equals( unauditedAttributeType )
				);
			if( !isAudited ) {
				return false;
			}

			if( attr.ConstructorArguments.Length < 2 ) {
				return false;
			}

			TypedConstant typeArg = attr.ConstructorArguments[ 0 ];
			if( typeArg.Value == null ) {
				return false;
			}
			if( !memberSymbol.ContainingType.Equals( typeArg.Value ) ) {
				return false;
			}

			TypedConstant nameArg = attr.ConstructorArguments[ 1 ];
			if( nameArg.Value == null ) {
				return false;
			}
			if( !memberSymbol.Name.Equals( nameArg.Value ) ) {
				return false;
			}

			return true;
		}

		private void ReportDiagnostic(
				SyntaxNodeAnalysisContext context,
				ISymbol memberSymbol,
				DiagnosticDescriptor diagnosticDescriptor
			) {

			Location location = context.ContainingSymbol.Locations[ 0 ];
			string methodName = memberSymbol.ToDisplayString( MemberDisplayFormat );

			var diagnostic = Diagnostic.Create(
					diagnosticDescriptor,
					location,
					methodName
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
