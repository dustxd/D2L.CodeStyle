using D2L.CodeStyle.TestAnalyzers.Common;
using D2L.CodeStyle.TestAnalyzers.Moq;
using D2L.CodeStyle.TestAnalyzers.Test.Verifiers;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using NUnit.Framework;

namespace D2L.CodeStyle.TestAnalyzers.NUnit {
	[TestFixture]
	internal sealed class ArgumentMatcherAnalyzerTests : DiagnosticVerifier {

		private const string PREAMBLE = @"
namespace Moq {
	public static class It {
		public static TValue Is<TValue>( Expression<Func<TValue, bool>> match ) { return default( TValue ); }
		public static TValue Is<TValue>( Expression<Func<object, Type, bool>> match ) { return default( TValue ); }
		public static TValue IsAny<TValue>() { return default( TValue ); }
		public static TValue IsIn<TValue>( IEnumerable<TValue> items ) { return default( TValue ); }
		public static TValue IsIn<TValue>( params TValue[] items ) { return default( TValue ); }
		public static TValue IsInRange<TValue>( TValue from, TValue to, Range rangeKind ) where TValue : IComparable { return default( TValue ); }
		public static TValue IsNotIn<TValue>( IEnumerable<TValue> items ) { return default( TValue ); }
		public static TValue IsNotIn<TValue>( params TValue[] items ) { return default( TValue ); }
		public static TValue IsNotNull<TValue>() { return default( TValue ); }
	}
}
";
		private static readonly int PREAMBLE_LINES = PREAMBLE.Split( '\n' ).Length;

		[Test]
		public void Matches_Argument_Type_NoDiagnostic() {

			const string test = PREAMBLE + @"
namespace TestNamespace {
	public class TestClass {
		public void Test() {
			System.IEqualityComparer<string> test = null;
			test.Equals( Moq.It.IsAny<string>() );
		}
	}
}";
			AssertNoDiagnostic(
				file: test
			);
		}

		[Test]
		public void Matches_Argument_ImplicitType_NoDiagnostic() {

			const string test = PREAMBLE + @"
namespace TestNamespace {
	public class TestClass {
		public void Test() {
			System.IEquatable<string> test = null;
			test.Equals( Moq.It.IsAny<StringWrapper>() );
		}
	}
	public class StringWrapper {
		public static implicit operator string( StringWrapper wrapper ) {
			return wrapper.ToString();
		}
	}
}";
			AssertSingleDiagnostic(
				diag: Diagnostics.TestMoqArgumentMatcherTypeMismatch,
				file: test,
				line: PREAMBLE_LINES + 5,
				column: 17,
				messageArgs: new[] {
					"Moq.It.IsAny<StringWrapper>()",
					"System.String"
				}
			);
		}

		private void AssertNoDiagnostic( string file ) {
			VerifyCSharpDiagnostic( sources: new[] { file } );
		}

		private void AssertSingleDiagnostic(
				DiagnosticDescriptor diag,
				string file,
				int line,
				int column,
				params object[] messageArgs
			) {

			DiagnosticResult result = new DiagnosticResult {
				Id = diag.Id,
				Message = string.Format( diag.MessageFormat.ToString(), messageArgs ),
				Severity = DiagnosticSeverity.Error,
				Locations = new[] {
					new DiagnosticResultLocation( "Test0.cs", line, column )
				}
			};

			VerifyCSharpDiagnostic(
					sources: new[] { file },
					expected: result
				);
		}
		protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer() {
			return new ArgumentMatcherAnalyzer();
		}
	}
}
