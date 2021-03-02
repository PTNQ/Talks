import AWS from 'aws-sdk';

/**
 * The singleton class that manages the dynamo db API
 */
class Dynamo {
  private static instance: AWS.DynamoDB;

  /**
   * The Singleton's constructor.
   */
  private constructor() {}

  /**
   * Function that returns the instance of the database to use.
   * @return {AWS.DynamoDB} An instance of the dynamo DB api.
   */
  public static getInstance(): AWS.DynamoDB {
    if (!Dynamo.instance) {
      AWS.config.update({ region: 'Local' });
      Dynamo.instance = new AWS.DynamoDB({
        apiVersion: '2012-08-10',
        endpoint: 'http://localhost:8000',
      });
    }
    return Dynamo.instance;
  }
}

export default Dynamo;
