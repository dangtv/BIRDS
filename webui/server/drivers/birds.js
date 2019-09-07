const uuid = require('uuid');
const { debug } = require('../lib/config').getPreDbConfig();
const utils = require('./utils');
const getMeta = require('../lib/getMeta');

const drivers = {};

/**
 * Run datalog for connection
 * Should return { rows, incomplete }
 * @param {string} datalog
 * @param {object} connection
 */
function birds(datalog, connection, timeout, verification, optimization, speedup) {
  if ((!connection) || (!connection.username)) {
    return new Promise((resolve, reject) => {
      var exec = require('child_process').exec;
      var command = 'echo "'+datalog+'" | birds -t '+timeout;
      if (verification) command = command + ' -v';
      if (optimization) command = command + ' -i -e ';
      if (speedup) command = command + ' -u ';
      exec(command, function callback(error, stdout, stderr){
        // console.log(stdout);
        // console.log(error);
        // console.log(stderr);
        if (error == null) {
          return resolve(stdout);
        }
        else {
          return reject(stderr);
          // return resolve(stdout);
        }
        });
    });
  }
  else {
    const pgConfig = {
      user: connection.username,
      password: connection.password,
      database: connection.database,
      host: connection.host,
      ssl: connection.postgresSsl
    };
    // TODO cache key/cert values
    if (connection.postgresKey && connection.postgresCert) {
      pgConfig.ssl = {
        key: fs.readFileSync(connection.postgresKey),
        cert: fs.readFileSync(connection.postgresCert)
      };
      if (connection.postgresCA) {
        pgConfig.ssl['ca'] = fs.readFileSync(connection.postgresCA);
      }
    }
    if (connection.port) pgConfig.port = connection.port
    else pgConfig.port='5432';

    return new Promise((resolve, reject) => {
      var exec = require('child_process').exec;
      var command = 'echo "'+datalog+'" | birds -c -h '+pgConfig.host+' -U '+pgConfig.user+' -p '+ pgConfig.port+' -w '
      +pgConfig.password+' -d '+pgConfig.database+ ' -t '+timeout;
      if (verification) command = command + ' -v';
      if (optimization) command = command + ' -i -e ';
      if (speedup) command = command + ' -u ';
      exec(command, function callback(error, stdout, stderr){
        // console.log(stdout);
        // console.log(error);
        // console.log(stderr);
        if (error == null) {
          return resolve(stdout);
        }
        else {
          return reject(stderr);
          // return resolve(stdout);
        }
        });
    });
  }
}

/**
 * Run datalog program using driver implementation of connection
 * @param {*} datalog
 * @param {*} connection
 * @param {object} [user] user may not be provided if chart links turned on
 * @param {*} timeout
 * @returns {Promise}
 */
function runDatalog(datalog, connection, user, timeout, verification, optimization, speedup) {
  // console.log (datalog+' hehee');
  const datalogResult = {
    id: uuid.v4(),
    cacheKey: null,
    startTime: new Date(),
    stopTime: null,
    datalogRunTime: null,
    fields: [],
    incomplete: false,
    meta: {},
    rows: []
  };

  return birds(datalog, connection, timeout, verification, optimization, speedup).then(results => {
    // const { rows, incomplete } = results;
    // console.log(results);

    datalogResult.sql = results;
    // datalogResult.incomplete = incomplete || false;
    // datalogResult.rows = rows;
    // datalogResult.rows = [{'SQL':results}];
    datalogResult.stopTime = new Date();
    datalogResult.datalogRunTime = datalogResult.stopTime - datalogResult.startTime;
    // datalogResult.meta = getMeta(rows);
    // datalogResult.fields = Object.keys(datalogResult.meta);
    datalogResult.fields = ['SQL'];

    if (debug) {
      var connectionName = "No connection";
      if (connection) if (connection.name) connectionName = connection.name;
      // const rowCount = rows.length;
      const { startTime, stopTime, datalogRunTime } = datalogResult;
      console.log("here");
      console.log(
        JSON.stringify({
          userId: user && user._id,
          userEmail: user && user.email,
          connectionName,
          timeout,
          startTime,
          stopTime,
          datalogRunTime,
          // rowCount,
          datalog
        })
      );
    }

    return datalogResult;
  });
}

/**
 * Test connection passed in using the driver implementation
 * As long as promise resolves without error
 * it is considered a successful connection config
 * @param {object} connection
 */
function testConnection(connection) {
  const driver = drivers[connection.driver];
  return driver.testConnection(connection);
}

/**
 * Gets schema (sometimes called schemaInfo) for connection
 * This data is used by client to build schema tree in editor sidebar
 * @param {object} connection
 * @returns {Promise}
 */
function getSchema(connection) {
  connection.maxRows = Number.MAX_SAFE_INTEGER;
  const driver = drivers[connection.driver];
  return driver.getSchema(connection);
}

/**
 * Gets array of driver objects
 * @returns {array} drivers
 */
function getDrivers() {
  return Object.keys(drivers).map(id => {
    return {
      id,
      name: drivers[id].name,
      fields: drivers[id].fields
    };
  });
}

/**
 * Validates connection object based on its driver
 * Unnecessary fields will be stripped out
 * @param {object} connection
 */
function validateConnection(connection) {
  const coreFields = ['_id', 'name', 'driver', 'createdDate', 'modifiedDate'];
  if (!connection.name) {
    throw new Error('connection.name required');
  }
  if (!connection.driver) {
    throw new Error('connection.driver required');
  }
  const driver = drivers[connection.driver];
  if (!driver) {
    throw new Error(`driver implementation ${connection.driver} not found`);
  }
  const validFields = driver.fields.map(field => field.key).concat(coreFields);
  const cleanedConnection = validFields.reduce(
    (cleanedConnection, fieldKey) => {
      if (connection.hasOwnProperty(fieldKey)) {
        let value = connection[fieldKey];
        const fieldDefinition =
          drivers[connection.driver].fieldsByKey[fieldKey];

        // field definition may not exist since
        // this could be a core field like _id, name
        if (fieldDefinition) {
          if (fieldDefinition.formType === 'CHECKBOX') {
            value = utils.ensureBoolean(value);
          }
        }

        cleanedConnection[fieldKey] = value;
      }
      return cleanedConnection;
    },
    {}
  );

  return cleanedConnection;
}

module.exports = {
  getDrivers,
  getSchema,
  runDatalog,
  testConnection,
  validateConnection
};
