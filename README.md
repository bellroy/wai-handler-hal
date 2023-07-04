# wai-handler-hal

[![Haskell-CI](https://github.com/bellroy/wai-handler-hal/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bellroy/wai-handler-hal/actions/workflows/haskell-ci.yml)

This library lets you run `wai` `Application`s on AWS Lambda, which
means you can now use mature web frameworks like
[`servant`](https://hackage.haskell.org/package/servant).

The main entry point is `Network.Wai.Handler.Hal.run`, which wraps an
`Application` and returns a function that can be passed to `hal`'s
`AWS.Lambda.Runtime.mRuntime`.

**NOTE:** The function returned by `Network.Wai.Handler.Hal.run` is
only for [Lambda Proxy
Integrations](https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html)
of AWS API Gateway **REST APIs** (`AWS::ApiGateway::RestApi` in
CloudFormation). If you try to use such a Lambda with an API Gateway
**HTTP API** (`AWS::ApiGatewayV2::Api` in CloudFormation), it will
return 500s.


For code examples, see [this repository](https://github.com/bellroy/wai-handler-hal-example),
which contains:

* [`wai-handler-hal-example`](https://github.com/bellroy/wai-handler-hal-example): A simple `servant` API set up to run on both `hal` and `warp`; and
* [`wai-handler-hal-cdk`](https://github.com/bellroy/wai-handler-hal-example/tree/master/wai-handler-hal-cdk): A small [AWS
  CDK](https://docs.aws.amazon.com/cdk/latest/guide/home.html)
  application that deploys an API backed by `wai-handler-hal-example`.

## Developing

Instead of serving your application with something like
[`warp`](https://hackage.haskell.org/package/warp), you will set up an
executable that serves your application with
[`hal`](https://hackage.haskell.org/package/hal). The necessary glue
code looks something like this:

```haskell
import AWS.Lambda.Runtime (mRuntime)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Hal as WaiHandler

app :: Application
app = undefined -- From Servant or wherever else

main :: IO ()
main = mRuntime $ WaiHandler.run app
```

### Local testing

Compiled Lambda functions can be awkward to test, especially if you're
relying on an API Gateway integration to translate HTTP
requests. Consider defining a second executable target that serves
your `Application` using `warp`, which you can use for local
testing.

### Caveats

* The Lambda is never told the port that the API Gateway is listening
  on. Most APIs will listen on `443` (HTTPS), so that's what the
  library reports by default. See `runWithContext` if you need to
  change this.

* The Lambda is never told the HTTP version that the client uses when
  talking with the API Gateway. We assume HTTP 1.1.

## Packaging

Lambda functions are packaged in one of two ways: as `.zip` files or
as Docker container images. The [`wai-handler-hal-cdk` example](https://github.com/bellroy/wai-handler-hal-example/tree/master/wai-handler-hal-cdk) uses
`.zip` files for simplicity. CDK doesn't give us an easy way to build
container images using `nix build`, so a container-based deployment
using CDK would need to push to ECR first and then reference the image
by name.

### `.zip` files

You will need to create a `.zip` file containing ([at
least](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html#runtimes-custom-build))
an executable named `bootstrap`. This executable needs to run in
Amazon's runtime environment, and there are multiple ways to ensure
this.

#### Static linking

IOHK's [`haskell.nix`](https://github.com/input-output-hk/haskell.nix)
can build static Haskell binaries by cross-compiling against musl
libc. This is convenient, but consider copyleft implications (of
`libgmp`; `wai-handler-hal` is BSD-3-Clause) if you are distributing
the binaries to other people.

#### Dynamic linking

The other option is to compile the executable in an environment with
packages that match the [Lambda runtime
environment](https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html). Some
ideas:

* Build the executable in an "imitation environment" like the [`lambci/lambda`](https://hub.docker.com/r/lambci/lambda) Docker container; or
* (Untested) build the executable on an Amazon Linux 2 EC2 instance.

### Docker container images

It is possible to package Lambdas into Docker containers for
deployment. Amazon provide base containers for custom runtimes in the
repository
[`amazon/aws-lambda-provided`](https://hub.docker.com/r/amazon/aws-lambda-provided/). One
nice feature of these images is that they provide a [runtime interface
emulator](https://docs.aws.amazon.com/lambda/latest/dg/images-test.html#images-test-AWSbase),
which they fall back to when not running "for real". This makes it
possible to directly invoke the lambda and see how it behaves. (This
is less important here because we can serve our application off
`warp`, but for writing Lambdas not invoked by an API Gateway Proxy
Integration, it's handy.)

At the time of writing (2021-04-06), the
`amazon/aws-lambda-provided:al2` (Amazon Linux 2 tag) is the newest
base image that Amazon recommends for custom runtimes. You can use a
`Dockerfile` to build your images; there are also commented .nix files
in the [example repository](https://github.com/bellroy/wai-handler-hal-example),
showing a couple of different approaches to building container images using Nix.

## Integrating

Actually calling the Lambda is done with a [Lambda proxy
integration](https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html)
from an [API Gateway REST
API](https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-rest-api.html).

The simplest possible integration sends every request to the Lambda
(where the `wai` `Application` can return 404 or whatever if the
endpoint doesn't match). This is done by mapping the paths `/` and
`/{proxy+}` for the HTTP method `ANY`. We do this in our [CDK example](https://github.com/bellroy/wai-handler-hal-example/tree/master/wai-handler-hal-cdk),
and [CDK provides a Construct](https://docs.aws.amazon.com/cdk/api/v2/docs/aws-cdk-lib.aws_apigateway.LambdaRestApi.html) which encapsulates this pattern, making
it extremely simple to deploy.

### Splitting Servant applications

While it's simplest to back the entire API with a single Lambda, it
does mean that you risk one Lambda carrying an ever-expanding set of
IAM permissions. Servant's combinators make it easy to break the API
into parts that are served by individual Lambdas, which can each have
the minimal set of permissions attached to their role. If your Servant
service had endpoints `foo`, `bar`, and `baz`, you'd serve them all
from Warp with an expression like `Warp.runEnv 8080 . Servant.serve $
foo :<|> bar :<|> baz`. But `Servant.serve foo`, `Servant.serve bar`,
and `Servant.serve baz` are also all WAI `Application`s, and can each
be bundled into distinct Lambda functions using `wai-handler-hal`.

### Other caveats

* The stage name in a path segment is not passed to the Lambda, so it
  is not passed to the `wai` `Application`. An invoke URL like
  `https://abcde12345.execute-api.us-east-1.amazonaws.com/prod/hoot`
  will send `pathInfo = ["hoot"]` to the `Application`.

## Formatters

The formatters used in this repo are provided by `shell.nix`:

* `*.hs`: [`ormolu`](https://github.com/tweag/ormolu)
* `*.cabal`:
  [`cabal-fmt`](https://hackage.haskell.org/package/cabal-fmt)
  (`cabal-fmt --inplace wai-handler-hal.cabal`)
* `*.nix`:
  [`nixpkgs-fmt`](https://github.com/nix-community/nixpkgs-fmt)
  (`nixpkgs-fmt *.nix`)

## Regenerate CI

This repo uses `haskell-ci`, which is provided by `flake.nix`:

```shell
haskell-ci regenerate
```