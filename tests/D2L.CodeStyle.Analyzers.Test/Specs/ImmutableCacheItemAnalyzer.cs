// analyzer: D2L.CodeStyle.Analyzers.ApiUsage.Caching.ImmutableCacheItemAnalyzer

using System;
using System.Collections.Generic;
using static D2L.CodeStyle.Annotations.Objects;

namespace D2L.LP.Caching {
	public interface ICache {
		T Add<T>( Func<T> createAndReturn, Func<T, string> keyGetter );
		T Get<T>( string key, Func<T> getter );
		void Remove( string key );
		T Set<T>( string key, Func<T> setAndReturn );
	}
}

namespace SpecTests {

	public sealed class MutableItem {
		public string Value { get; set; }
	}

	internal sealed class UnmarkedUsages {

		public void GetMutableItem() {
			D2L.LP.Caching.ICache cache = default;
			/* CacheItemTypeShouldBeImmutable(SpecTests.MutableItem) */	cache.Get( "abc", () => new MutableItem() ) /**/;
		}

		public void GetGenericItem<T>() where T : class, new() {
			D2L.LP.Caching.ICache cache = default;
			cache.Get<T>( "abc", () => new T() );
		}
	}
}
