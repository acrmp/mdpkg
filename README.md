# mdpkg

This is an experiment in providing a JSON view of different package
repositories.

The idea is to make package metadata easily consumable across a range of
package formats by normalising the metadata.

## Example

Given a document describing two repositories, one Ubuntu and one CentOS:

```json
{
  "distribution": {
    "name": "ubuntu",
    "release": "10.04",
    "platform_family": "debian",
    "url": "http://mirror.anl.gov/pub/ubuntu/dists/lucid/main/binary-amd64/Packages.bz2"
  },
  "distribution": {
    "name": "centos",
    "release": "5",
    "platform_family": "rhel",
    "url": "http://mirror.anl.gov/pub/centos/5/os/x86_64/repodata/primary.xml.gz"
  }
}
```

Then mdpkg should make some basic information available about each package:

```
$ curl 'http://metapackage.s3-website-us-east-1.amazonaws.com/ubuntu/10.04/apache2'
```
```json
{
  "package": {
    "name": "apache2",
    "architecture": "amd64",
    "version": "2.2.14-5ubuntu8"
  }
}
```
