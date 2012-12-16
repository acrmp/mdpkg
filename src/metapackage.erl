-module(metapackage).

-export([fetch_metadata/0, start/0]).

fetch_metadata() ->
    {packages,
     [extract_metadata(Source) || Source <- sources()]}.

extract_metadata(Source) ->
    {distribution, Distro} = Source,
    {ok, {_, _, Metadata}} = httpc:request(get,
					   {proplists:get_value(url, Distro),
					    []},
					   [], []),
    Packages = case
		 list_to_atom(proplists:get_value(platform_family,
						  Distro))
		   of
		 debian ->
		     [md_pkg_debian:normalise(X)
		      || X
			     <- md_pkg_debian:packages_from_stream(list_to_binary(Metadata))];
		 rhel ->
		     [md_pkg_redhat:normalise(X)
		      || X
			     <- md_pkg_redhat:packages_from_stream(list_to_binary(Metadata))]
	       end,
    {distribution, [{packages, Packages} | Distro]}.

sources() ->
    inets:start(),
    {ok, {_, _, Content}} = httpc:request(get,
					  {"http://metapackage.s3-website-us-east-1.amazo"
					   "naws.com/",
					   []},
					  [], []),
    F = fun (V) when is_binary(V) -> binary_to_list(V);
	    (V) -> V
	end,
    jsx:decode(list_to_binary(Content),
	       [{labels, atom}, {post_decode, F}]).

start() -> md_publisher_s3:publish(fetch_metadata()).
