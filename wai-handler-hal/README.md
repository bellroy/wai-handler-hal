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
