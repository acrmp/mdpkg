-module(md_pkg_redhat).

-export([normalise/1, packages/1, packages_from_file/1,
	 packages_from_stream/1]).

normalise(Package) ->
    {redhat_package, Rpm} = Package,
    {package,
     [normalise_field(F)
      || F <- Rpm, normalise_field(F) /= {}]}.

normalise_field({name, Name}) ->
    {name, list_to_binary(Name)};
normalise_field({version, Version}) ->
    {_, [H | T]} = lists:unzip(Version),
    {version,
     list_to_binary(string:join([H, string:join(T, "-")],
				":"))};
normalise_field({arch, Arch}) ->
    {architecture, list_to_binary(Arch)};
normalise_field({_, _}) -> {}.

packages_from_file(Filename) ->
    {ok, GzipCompressed} = file:read_file(Filename),
    packages_from_stream(GzipCompressed).

packages_from_stream(GzipCompressed) ->
    packages(binary_to_list(zlib:gunzip(GzipCompressed))).

packages(Metadata) ->
    {ok, State, _} = erlsom:parse_sax(Metadata, [],
				      fun callback/2),
    [{element, _}, {packages, Packages}] = State,
    lists:reverse(Packages).

callback(Event, S) ->
    Namespace = "http://linux.duke.edu/metadata/common",
    Fields = ["name", "arch", "summary", "description",
	      "url"],
    State = case S of
	      [] -> [{element, undefined}, {packages, []}];
	      _ -> S
	    end,
    [{element, Element}, {packages, Packages}] = State,
    case Event of
      startDocument -> State;
      {startElement, Namespace, "package", _, _} ->
	  NewPackages = [[] | Packages],
	  [{element, package}, {packages, NewPackages}];
      {endElement, Namespace, "package", _} ->
	  [Package | T] = Packages,
	  NewPackages = [{redhat_package, lists:reverse(Package)}
			 | T],
	  [{element, package}, {packages, NewPackages}];
      {startElement, Namespace, "version", _, Attributes} ->
	  [Package | T] = Packages,
	  NewPackage = [{version,
			 lists:reverse([version_field(A) || A <- Attributes])}
			| Package],
	  NewPackages = [NewPackage | T],
	  [{element, "version"}, {packages, NewPackages}];
      {startElement, Namespace, Field, _, _} ->
	  case lists:member(Field, Fields) of
	    true -> [{element, Field}, {packages, Packages}];
	    false -> State
	  end;
      {characters, Characters} ->
	  [Package | T] = Packages,
	  NewPackages = [case lists:member(Element, Fields) of
			   true ->
			       lists:ukeymerge(1, Package,
					       [{list_to_atom(Element),
						 Characters}]);
			   false -> Package
			 end
			 | T],
	  [{element, Element}, {packages, NewPackages}];
      _ -> State
    end.

version_field({attribute, Name, [], [], Version}) ->
    case lists:member(Name, ["epoch", "ver", "rel"]) of
      true -> {list_to_atom(Name), Version};
      false -> {}
    end.
