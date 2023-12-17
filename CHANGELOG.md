# Revision history for wai-handler-hal

## 0.3.0.0 -- 2023-12-17

- Breaking change: add `Options` record parameter to `runWithOptions`,
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
