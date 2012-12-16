-module(md_pkg_debian).

-export([normalise/1, packages/1, packages_from_file/1,
	 packages_from_stream/1]).

normalise(Package) ->
    {debian_package, Dpkg} = Package,
    {package,
     [normalise_field(F)
      || F <- Dpkg, normalise_field(F) /= {}]}.

normalise_field({<<"Package">>, Name}) -> {name, Name};
normalise_field({<<"Version">>, Version}) ->
    {version, Version};
normalise_field({<<"Architecture">>, Arch}) ->
    {architecture, Arch};
normalise_field({_, _}) -> {}.

packages_from_file(Filename) ->
    {ok, BzipCompressed} = file:read_file(Filename),
    packages_from_stream(BzipCompressed).

packages_from_stream(BzipCompressed) ->
    packages(binary_to_list(erlbz2:decompress(BzipCompressed))).

packages(Metadata) ->
    [package(P)
     || P <- re:split(Metadata, "\n\n"), P /= <<>>].

package(Metadata) ->
    [_ | Fields] = re:split(Metadata, "(?:^|\n)([^ :]+): "),
    {debian_package,
     [list_to_tuple(lists:sublist(Fields, F, 2))
      || F <- lists:seq(1, length(Fields), 2)]}.
