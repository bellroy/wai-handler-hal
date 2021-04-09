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

## Useful commands

 * `npm run build` --- compile typescript to js
 * `npm run watch` --- watch for changes and compile
 * `npm run cdk deploy` --- deploy this stack to your default AWS
   account/region. **Reminder:** Build the runtime first!
 * `npm run cdk diff` --- compare deployed stack with current state
 * `npm run cdk synth` --- emits the synthesized CloudFormation template
