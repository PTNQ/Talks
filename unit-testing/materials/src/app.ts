import Database from './database';
import { User } from './models/user';
import {
  GetItemInput,
  PutItemInput,
  PutItemOutput,
} from 'aws-sdk/clients/dynamodb';
import { AWSError } from 'aws-sdk';

/**
 * Main runner for dynamo db example.
 * @class Runner
 */
class Runner {
  private db: Database = new Database();

  /**
   * Adds a user to the database.
   * @param {User} user
   * @return {Promise<void>}
   */
  public addUser = (user: User): Promise<PutItemOutput> => {
    // Pre-processing of user object to prepare it
    // for adding it to the databse

    // Fill in undefined data
    if (!user.name) {
      user.name = '';
    }

    if (!user.email) {
      user.email = '';
    }

    if (!user.phone) {
      user.phone = '';
    }

    // Create object that we are sending into the db
    const params: PutItemInput = {
      Item: {
        name: { S: user.name },
        email: { S: user.email },
        phone: { S: user.phone },
      },
      ReturnConsumedCapacity: 'TOTAL',
      TableName: 'User',
    };

    // Tell the database to add the user
    return this.db
      .putItem(params)
      .then((res) => {
        console.log('Succesfully added user to dabatase!');
        return Promise.resolve(res);
      })
      .catch((err) => {
        console.error('Could not add user to database.');
        return Promise.reject(err);
      });
  };

  /**
   * Retreives a user from the database.
   * @param {string} name
   * @return {Promise<User | AWSError>}
   */
  public getUser = (name: string): Promise<User | AWSError> => {
    // Create object that we are gettin from the db
    const params: GetItemInput = {
      Key: { name: { S: name } },
      TableName: 'User',
    };

    // Tell the database to add the user
    return this.db
      .getItem(params)
      .then((data) => {
        let user: User = {};

        // Check if we returned an item
        if (data.Item) {
          user = {
            email: data.Item.email.S,
            name: data.Item.name.S,
            phone: data.Item.phone.S,
          };
          console.log('Succesfully retreived user from dabatase!');
          return Promise.resolve(user);
        } else {
          console.log('Database returned empty item');
          const err = new Error('Database returned no data.');
          return Promise.reject(err);
        }
      })
      .catch((err: AWSError) => {
        console.error('Could not add user to database.');
        return Promise.reject(err);
      });
  };
}

export default new Runner();
