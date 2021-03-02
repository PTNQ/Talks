import runner from '../src/app';
import { assert } from 'chai';
import 'mocha';
import Dynamo from '../src/dynamo';
import Sinon from 'sinon';
import { AWSError } from 'aws-sdk';
import {
  GetItemInput,
  GetItemOutput,
  PutItemInput,
  PutItemOutput,
} from 'aws-sdk/clients/dynamodb';
import { User } from '../src/models/user';

describe('User functions test', () => {
  let dynamo: AWS.DynamoDB;
  let sandbox: Sinon.SinonSandbox;

  beforeEach(() => {
    dynamo = Dynamo.getInstance();
    sandbox = Sinon.createSandbox();
  });

  afterEach(() => {
    // cleanup
    sandbox.restore();
  });

  it('should add a user to the db', () => {
    const user: User = {
      name: 'Matt',
      email: 'student@concordia.ca',
      phone: '5146663333',
    };

    const putItemOutput: PutItemOutput = {
      ConsumedCapacity: { TableName: 'User', CapacityUnits: 1 },
    };

    const putItemOutputResolves = ({
      promise() {
        return Promise.resolve(putItemOutput);
      },
    } as unknown) as AWS.Request<PutItemOutput, AWSError>;

    const params: PutItemInput = {
      Item: {
        name: { S: user.name },
        email: { S: user.email },
        phone: { S: user.phone },
      },
      ReturnConsumedCapacity: 'TOTAL',
      TableName: 'User',
    };

    sandbox
      .stub(dynamo, 'putItem')
      .withArgs(Sinon.match(params))
      .returns(putItemOutputResolves);

    return runner.addUser(user).then((res) => {
      assert.equal(res, putItemOutput);
    });
  });

  it('should NOT add a user to the db', () => {
    const awsError: AWSError = {
      code: 'badRequest',
      message: 'bad request',
      retryable: false,
      statusCode: 1,
      time: new Date(),
      name: '',
      hostname: '',
      region: '',
      retryDelay: 1,
      requestId: '',
      extendedRequestId: '',
      cfId: '',
    };

    const putItemOutputRejects = ({
      promise() {
        return Promise.reject(awsError);
      },
    } as unknown) as AWS.Request<PutItemOutput, AWSError>;

    const user: User = {
      name: 'Matt',
      email: 'student@concordia.ca',
      phone: '5146663333',
    };

    const params: PutItemInput = {
      Item: {
        name: { S: user.name },
        email: { S: user.email },
        phone: { S: user.phone },
      },
      ReturnConsumedCapacity: 'TOTAL',
      TableName: 'User',
    };

    sandbox
      .stub(dynamo, 'putItem')
      .withArgs(Sinon.match(params))
      .returns(putItemOutputRejects);

    return runner
      .addUser(user)
      .then(() => {
        assert.fail('add user should fail');
      })
      .catch((err) => {
        assert.equal(err, awsError);
      });
  });

  it('should get a user', () => {
    const user: User = {
      name: 'Matt',
      email: 'student@concordia.ca',
      phone: '5146663333',
    };

    const params: GetItemInput = {
      Key: { name: { S: user.name } },
      TableName: 'User',
    };

    const getItemOutput: GetItemOutput = {
      Item: {
        name: { S: user.name },
        email: { S: user.email },
        phone: { S: user.phone },
      },
      ConsumedCapacity: { TableName: 'User', CapacityUnits: 1 },
    };

    const getItemResolves = ({
      promise() {
        return Promise.resolve(getItemOutput);
      },
    } as unknown) as AWS.Request<GetItemOutput, AWSError>;

    sandbox
      .stub(dynamo, 'getItem')
      .withArgs(Sinon.match(params))
      .returns(getItemResolves);

    return runner.getUser(user.name || '').then((res: User) => {
      assert(res.name, user.name);
      assert(res.email, user.email);
      assert(res.phone, user.phone);
    });
  });

  it('should reject if no user found', () => {
    const user: User = {
      name: 'Matt',
      email: 'student@concordia.ca',
      phone: '5146663333',
    };

    const params: GetItemInput = {
      Key: { name: { S: user.name } },
      TableName: 'User',
    };

    const getItemOutput: GetItemOutput = {
      ConsumedCapacity: { TableName: 'User', CapacityUnits: 1 },
    };

    const getItemResolves = ({
      promise() {
        return Promise.resolve(getItemOutput);
      },
    } as unknown) as AWS.Request<GetItemOutput, AWSError>;

    sandbox
      .stub(dynamo, 'getItem')
      .withArgs(Sinon.match(params))
      .returns(getItemResolves);

    return runner
      .getUser(user.name || '')
      .then((res: User) => {
        assert.fail('Should fail getting user');
      })
      .catch((err) => {
        assert.equal(err.message, 'Database returned no data.');
      });
  });

  it('should NOT get a user', () => {
    const awsError: AWSError = {
      code: 'badRequest',
      message: 'bad request',
      retryable: false,
      statusCode: 1,
      time: new Date(),
      name: '',
      hostname: '',
      region: '',
      retryDelay: 1,
      requestId: '',
      extendedRequestId: '',
      cfId: '',
    };

    const user: User = {
      name: 'Matt',
      email: 'student@concordia.ca',
      phone: '5146663333',
    };

    const params: GetItemInput = {
      Key: { name: { S: user.name } },
      TableName: 'User',
    };

    const getItemResolves = ({
      promise() {
        return Promise.reject(awsError);
      },
    } as unknown) as AWS.Request<GetItemOutput, AWSError>;

    sandbox
      .stub(dynamo, 'getItem')
      .withArgs(Sinon.match(params))
      .returns(getItemResolves);

    return runner
      .getUser(user.name || '')
      .then((res: User) => {
        assert.fail('should fail get the user');
      })
      .catch((err) => {
        assert.equal(err, awsError);
      });
  });
});
