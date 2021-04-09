import * as cdk from '@aws-cdk/core';
import * as apigateway from '@aws-cdk/aws-apigateway';
import * as lambda from '@aws-cdk/aws-lambda';

export class WaiHandlerHalCdkStack extends cdk.Stack {
  constructor(scope: cdk.Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const exampleLambda = new lambda.Function(this, 'wai-handler-hal-example', {
      code: lambda.Code.fromAsset('runtime/result'),
      handler: 'UNUSED',
      runtime: lambda.Runtime.PROVIDED_AL2,
    })

    const exampleApi = new apigateway.LambdaRestApi(
      this,
      'wai-handler-hal-api', {
        handler: exampleLambda
      }
    );
  }
}
