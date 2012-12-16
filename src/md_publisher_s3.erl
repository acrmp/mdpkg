-module(md_publisher_s3).

-export([publish/1]).

publish(DistroPackages) ->
    {packages, Distributions} = DistroPackages,
    erlcloud:start(),
    [publish_packages(D) || D <- Distributions].

publish_packages({distribution, Distro}) ->
    Name = proplists:get_value(name, Distro),
    Release = proplists:get_value(release, Distro),
    Packages = proplists:get_value(packages, Distro),
    publish_resource(string:join([Name, "index.json"], "/"),
		     Packages),
    publish_resource(string:join([Name, Release,
				  "index.json"],
				 "/"),
		     Packages),
    [publish_resource(string:join([Name, Release,
				   package_name(P)],
				  "/"),
		      [P])
     || P <- Packages].

package_name({package, Package}) ->
    binary_to_list(proplists:get_value(name, Package)).

publish_resource(Path, Content) ->
    erlcloud_s3:put_object("metapackage", Path,
			   jsx:prettify(jsx:encode(Content)), [],
			   [{"x-amz-acl", "public-read"},
			    {"content-type", "application/json"}]).
