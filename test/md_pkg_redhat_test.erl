-module(md_pkg_redhat_test).

-include_lib("eunit/include/eunit.hrl").

normalise_test() ->
    [?assertError({badmatch, _},
		  (md_pkg_redhat:normalise({debian_package, []}))),
     ?assertEqual({package, []},
		  (md_pkg_redhat:normalise({redhat_package, []}))),
     ?assertEqual({package, [{name, <<"foo">>}]},
		  (md_pkg_redhat:normalise({redhat_package,
					    [{name, "foo"}]}))),
     ?assertEqual({package,
		   [{name, <<"foo">>}, {version, <<"0:47-7.el6">>}]},
		  (md_pkg_redhat:normalise({redhat_package,
					    [{name, "foo"},
					     {version,
					      [{epoch, "0"}, {ver, "47"},
					       {rel, "7.el6"}]}]}))),
     ?assertEqual({package,
		   [{name, <<"foo">>}, {architecture, <<"noarch">>}]},
		  (md_pkg_redhat:normalise({redhat_package,
					    [{name, "foo"},
					     {arch, "noarch"}]}))),
     ?assertEqual({package, [{name, <<"lemon">>}]},
		  (md_pkg_redhat:normalise({redhat_package,
					    [{name, "lemon"},
					     {taste, "sour"}]})))].

packages_test() ->
    [?assertThrow({error, _},
		  (md_pkg_redhat:packages("not xml"))),
     ?assertEqual([],
		  (md_pkg_redhat:packages("<different_document_type/>"))),
     ?assertEqual([],
		  (md_pkg_redhat:packages("<metadata xmlns=\"http://linux.duke.edu/metad"
					  "ata/common\"/>"))),
     ?assertEqual([{redhat_package,
		    [{name, "gutenprint-plugin"}, {arch, "x86_64"},
		     {version,
		      [{epoch, "0"}, {ver, "5.2.5"}, {rel, "2.el6"}]},
		     {url, "http://gimp-print.sourceforge.net/"},
		     {summary, "GIMP plug-in for gutenprint"},
		     {description,
		      "This package contains the gutenprint "
		      "GIMP plug-in."}]}],
		  (md_pkg_redhat:packages("<metadata xmlns=\"http://linux.duke.edu/metad"
					  "ata/common\" xmlns:rpm=\"http://linux.duke.ed"
					  "u/metadata/rpm\" packages=\"1\">\n<package "
					  "type=\"rpm\">\n  <name>gutenprint-plugin</nam"
					  "e>\n  <arch>x86_64</arch>\n  <version "
					  "epoch=\"0\" ver=\"5.2.5\" rel=\"2.el6\"/>\n "
					  " <checksum type=\"sha256\" pkgid=\"YES\">471b"
					  "de7c438c832223b4047d7f244d5ca0df6476b1e97e836"
					  "5dc2c435b39342a</checksum>\n  <summary>GIMP "
					  "plug-in for gutenprint</summary>\n  "
					  "<description>This package contains the "
					  "gutenprint GIMP plug-in.</description>\n "
					  " <packager>CentOS BuildSystem &lt;http://bugs"
					  ".centos.org&gt;</packager>\n  <url>http://gim"
					  "p-print.sourceforge.net/</url>\n  <time "
					  "file=\"1309666876\" build=\"1289520702\"/>\n "
					  " <size package=\"18888\" installed=\"20816\" "
					  "archive=\"21092\"/>\n</package>\n</metadata>")))].
