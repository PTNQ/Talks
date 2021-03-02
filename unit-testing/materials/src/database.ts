import { AWSError } from 'aws-sdk';
import {
  PutItemInput,
  PutItemOutput,
  GetItemInput,
  GetItemOutput,
} from 'aws-sdk/clients/dynamodb';
import Dynamo from './dynamo';

/**
 * A wrapper for dynamo db.
 * @class Database
 */
export default class Database {
  private db: AWS.DynamoDB;

  /**
   * @constructor
   */
  constructor() {
    this.db = Dynamo.getInstance();
  }

  /**
   * Add an item to a table in dynamo.
   * @param {PutItemInput} params
   * @return {Promise<PutItemOutput>} A promise.
   */
  public putItem = (params: PutItemInput): Promise<PutItemOutput> => {
    return this.db
      .putItem(params) // to be mocked
      .promise()
      .then()
      .catch((err: AWSError) => {
        // console.error('Could not put item in', params.TableName);
        return Promise.reject(err);
      });
  };

  /**
   * Retreive an item from a table in dynamo.
   * @param {GetItemInput} params
   * @return {Promise<GetItemOutput>} A promise.
   */
  public getItem = (params: GetItemInput): Promise<GetItemOutput> => {
    return this.db
      .getItem(params) // to be mocked
      .promise()
      .catch((err: AWSError) => {
        // console.error('Could not get item from', params.TableName);
        return Promise.reject(err);
      });
  };
}
