-module(md_pkg_debian_test).

-include_lib("eunit/include/eunit.hrl").

normalise_test() ->
    [?assertError({badmatch, _},
		  (md_pkg_debian:normalise({redhat_package, package}))),
     ?assertEqual({package, []},
		  (md_pkg_debian:normalise({debian_package, []}))),
     ?assertEqual({package, [{name, <<"Foo">>}]},
		  (md_pkg_debian:normalise({debian_package,
					    [{<<"Package">>, <<"Foo">>}]}))),
     ?assertEqual({package,
		   [{name, <<"Foo">>}, {version, <<"1.2.3">>}]},
		  (md_pkg_debian:normalise({debian_package,
					    [{<<"Package">>, <<"Foo">>},
					     {<<"Version">>, <<"1.2.3">>}]}))),
     ?assertEqual({package,
		   [{name, <<"Foo">>}, {architecture, <<"x86">>}]},
		  (md_pkg_debian:normalise({debian_package,
					    [{<<"Package">>, <<"Foo">>},
					     {<<"Architecture">>,
					      <<"x86">>}]}))),
     ?assertEqual({package, [{name, <<"Foo">>}]},
		  (md_pkg_debian:normalise({debian_package,
					    [{<<"Package">>, <<"Foo">>},
					     {<<"DeliveryDate">>,
					      <<"2012-12-16">>}]})))].

packages_test() ->
    [?assertEqual([], (md_pkg_debian:packages(""))),
     ?assertEqual([{debian_package,
		    [{<<"Package">>, <<"abrowser">>},
		     {<<"Priority">>, <<"optional">>},
		     {<<"Section">>, <<"web">>},
		     {<<"Installed-Size">>, <<"136">>}]}],
		  (md_pkg_debian:packages("Package: abrowser\nPriority: optional\nSectio"
					  "n: web\nInstalled-Size: 136"))),
     ?assertEqual([{debian_package,
		    [{<<"Package">>, <<"foo">>}]},
		   {debian_package, [{<<"Package">>, <<"bar">>}]}],
		  (md_pkg_debian:packages("Package: foo\n\nPackage: bar")))].
