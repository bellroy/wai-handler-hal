# CDK TypeScript example for `wai-handler-hal`

This is a simple CDK project that deploys a Lambda based on
`wai-handler-hal` to serve an entire API Gateway REST API.

The Haskell binary is stripped and statically linked againt musl libc,
thanks to IOHK's
[`haskell.nix`](https://github.com/input-output-hk/haskell.nix). It is
also compressed with [`upx`](https://upx.github.io/).

The `cdk.json` file tells the CDK Toolkit how to execute the app.

## Quick deploy

The project's `shell.nix` provides `npm` and `nodejs`, so run the
following commands from inside a `nix-shell`:

* `npm install`
* `npm run cdk bootstrap` (only if you've never used CDK on your AWS
  account before)
* `(cd runtime && nix build)` (build the Lambda binary where CDK can
  find it)
* `npm run cdk deploy` (deploy to AWS)

To tear down the stack, run `npm run cdk destroy`. If you don't plan
on using CDK in your account any more, remember to remove its
bootstrap CloudFormation stack, and manually delete any leftover
resources (like S3 buckets).

## Building the binary

`runtime/default.nix` is a Nix expression to build the
statically-linked `hal` binary, and the CDK script is configured to
expect it at `runtime/result/bootstrap`. There are two other example
`.nix` files in that directory, if you want to explore deploying
Lambda functions using OCI images on
[ECR](https://aws.amazon.com/ecr/):

* `runtime/container.nix` builds a container from Amazon's
  [`al2`](https://hub.docker.com/r/amazon/aws-lambda-provided/tags)
  container. It places the statically-linked binary into the correct
  location in the container filesystem. Load with `docker load <
  result`.
* `runtime/tiny-container.nix` builds a minimal container from four
  parts:
  - The statically-linked Haskell binary;
  - The [Lambda Runtime Interface
    Emulator](https://github.com/aws/aws-lambda-runtime-interface-emulator/);
  - A `lambda-entrypoint.sh` to run the emulator if the container is
    not running in the AWS Cloud; and
  - A statically-linked `busybox`, to provide a minimal shell to run
    the entrypoint script.
  Load with `./result | docker load`

### Thoughts on the container format and the Lambda Runtime Interface Emulator

This emulator simulates the Lambda runtime environment, so that you
can run your functions locally and invoke them using `curl`. If you
can run your web service using `warp`, the emulator is probably not
especially useful. But if you're directly invoking your Lambda
Function, or have it connected to other event sources, it can be
helpful to capture some payloads and replay them locally. You can then
insert debug prints without redeploying, waiting for CloudWatch Logs,
etc.

If you can make working static binaries, remapping the port used by
the emulator looks like the only real benefit to making a container,
and so zip-based Lambda Functions appear perfectly acceptable for
deployment. Besides, if you're only testing one Lambda Function at a
time, you can download the emulator and run it straight from the
shell.

## Useful commands

 * `npm run build` --- compile typescript to js
 * `npm run watch` --- watch for changes and compile
 * `npm run cdk deploy` --- deploy this stack to your default AWS
   account/region. **Reminder:** Build the runtime first!
 * `npm run cdk diff` --- compare deployed stack with current state
 * `npm run cdk synth` --- emits the synthesized CloudFormation template
