using System.Collections.Generic;
using System.Collections.Immutable;

namespace D2L.CodeStyle.Analyzers.ApiUsage.Caching {

	internal static class CacheMethods {

		internal static readonly IReadOnlyDictionary<string, ImmutableArray<string>> Definitions =
			ImmutableDictionary.Create<string, ImmutableArray<string>>()
			.Add(
				"D2L.LP.Caching.ICache",
				ImmutableArray.Create(
					"Add",
					"Get",
					"Set"
				)
			)
			.Add(
				"D2L.LP.Caching.ILocalCache",
				ImmutableArray.Create(
					"TryGet"
				)
			)
			.Add(
				"D2L.LP.Caching.ISmartCache",
				ImmutableArray.Create(
					"AddLocally",
					"TryGetLocally"
				)
			);
	}
}
