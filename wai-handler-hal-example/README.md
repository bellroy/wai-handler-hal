# Example binary for wai-handler-hal

This package shows how to run a Servant API on AWS Lambda. Servant
returns a WAI `Application`, which we wrap with `wai-handler-hal` and
run with `hal`'s '`mRuntimeWithContext`. This allows us to deploy the
binary to AWS Lambda, and use it as a [Lambda Proxy
Integration](https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html)
of an AWS API Gateway **REST API** (only).

In addition, this package provides a second executable target, which
runs the same WAI `Application` on the
[`warp`](https://hackage.haskell.org/package/warp) web server. This is
great for local testing.

Two endpoints are provided:

* `GET https://abcde12345.execute-api.us-east-1.amazonaws.com/prod/hoot` --- returns `{ "message": "hoot" }`
* `GET https://abcde12345.execute-api.us-east-1.amazonaws.com/prod/greet?person=Roy` --- returns `{ "message": "Hello, Roy!" }`
