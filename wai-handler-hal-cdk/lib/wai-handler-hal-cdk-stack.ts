import { Construct } from 'constructs';
import { Stack, StackProps } from 'aws-cdk-lib';
import {
  aws_apigateway as apigateway,
  aws_lambda as lambda
} from 'aws-cdk-lib';

export class WaiHandlerHalCdkStack extends Stack {
  constructor(scope: Construct, id: string, props?: StackProps) {
    super(scope, id, props);

    const exampleLambda = new lambda.Function(this, 'wai-handler-hal-example', {
      architecture: lambda.Architecture.X86_64,
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
