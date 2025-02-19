# Revision history for wai-handler-hal

## 0.4.0.1 -- 2025-02-19

- When resolving source IPs, do not require `AF_INET` (IPv4)
  addresses. This allows IPv6 source addresses to be passed through to
  the underlying `wai` `Application`.

## 0.4.0.0 -- 2024-01-17

- New function: `Wai.Handler.Hal.runWithOptions :: Options ->
  Application -> ProxyRequest NoAuthorizer -> ProxyResponse`. This
  provides a convenient way to pass custom `Options` without all the
  bells and whistles of `runWithContext`.

- Instead of guessing whether a given response `Content-Type` should
  be sent as text or base64-encoded binary, `Options` now contains a
  `binaryMediaTypes :: [MediaType]`, which lists the media types that
  should be base64-encoded. This should match the `binaryMediaTypes`
  setting you have configured on the API Gateway that integrates with
  your Lambda Function.

  _See:_ [Content type conversion in API
    Gateway](https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-payload-encodings-workflow.html)
    in the [Amazon API Gateway Developer
    Guide](https://docs.aws.amazon.com/apigateway/latest/developerguide/).

## 0.3.0.0 -- 2023-12-17

- Accidental breaking change: more elaborate `Content-Type` headers
  like `Content-Type: application/json; charset=utf-8` are now encoded
  as if they were binary payloads. This release has been deprecated.
- Breaking change: add `Options` record parameter to `runWithContext`,
  `toWaiRequest` and `fromWaiResponse`.
- Provide a `defaultOptions`.
- Make whether or not to run base64-encoding on the response body customizable
  through `Options.binaryMimeType`.

## 0.2.0.0 -- 2023-03-17

- Breaking change: `toWaiRequest` now sorts request headers and query string
  parameters.

## 0.1.2.0 -- 2022-06-03

- Fix construction of `rawPathInfo`.

## 0.1.1.0 -- 2021-10-14

- When API Gateway sends nonsense IPs during a test invocation, sub in
  `127.0.0.1` instead of exploding.
  [#3](https://github.com/bellroy/wai-handler-hal/issues/3)
- Removed debug `print`s left in by mistake.

## 0.1.0.0 -- 2021-04-15

- First version. Released on an unsuspecting world.
