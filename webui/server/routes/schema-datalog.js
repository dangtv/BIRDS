const router = require('express').Router();
const connections = require('../models/connections');
const Cache = require('../models/Cache.js');
const driver = require('../drivers');
const mustBeAuthenticated = require('../middleware/must-be-authenticated.js');
const sendError = require('../lib/sendError');

/**
 * Run datalog for connection
 * Should return { rows, incomplete }
 * @param {string} datalog
 * @param {object} connection
 */
function birds_import(connection) {
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
    exec('echo "" | birds -c --import -h '+pgConfig.host+' -U '+pgConfig.user+' -p '+ pgConfig.port+' -w '
    +pgConfig.password+' -d '+pgConfig.database, function callback(error, stdout, stderr){
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

router.get('/api/schema-datalog/:connectionId', mustBeAuthenticated, function(
  req,
  res
) {
  // const cacheKey = 'schemaCache:' + req.params.connectionId;
    return Promise.all(
      [connections.findOneById(req.params.connectionId)]
      // This has problems in TravisCI for some reason...
      // Cache.findOneByCacheKey(cacheKey)
    )
    .then(results => {
      let [conn] = results;
      if (!conn) {
        throw new Error('Connection not found');
      }
      
      return birds_import(conn).then(datalogSchema => {
        res.send({ datalogSchema });
      });
    })
    .catch(error => {
      if (error.message === 'Connection not found') {
        return sendError(res, error);
      }
      sendError(res, error, 'Problem getting schema info');
    });

  
});

module.exports = router;
