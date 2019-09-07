const sanitize = require('sanitize-filename');
const moment = require('moment');
const router = require('express').Router();
const { runQuery } = require('../drivers/index');
const { runDatalog } = require('../drivers/birds');
const connections = require('../models/connections.js');
const Cache = require('../models/Cache.js');
const Datalog = require('../models/Datalog.js');
const mustBeAuthenticated = require('../middleware/must-be-authenticated.js');
const mustBeAuthenticatedOrChartLink = require('../middleware/must-be-authenticated-or-chart-link-noauth.js');
const sendError = require('../lib/sendError');

// This allows executing a Datalog program relying on the saved Datalog text
// Instead of relying on an open endpoint that executes arbitrary sql
router.get(
  '/api/datalog-result/:_dataogId',
  mustBeAuthenticatedOrChartLink,
  function(req, res) {
    return Datalog.findOneById(req.params._datalogId)
      .then(datalog => {
        if (!datalog) {
          return sendError(res, null, 'Datalog not found (save query first)');
        }
        const data = {
          connectionId: datalog.connectionId,
          cacheKey: datalog._id,
          datalogName: datalog.name,
          datalogText: datalog.datalogText,
          config: req.config
        };
        // NOTE: Sends actual error here since it might have info on why the query is bad
        return getDatalogResult(data)
          .then(datalogResult => res.send({ datalogResult }))
          .catch(error => sendError(res, error));
      })
      .catch(error => sendError(res, error, 'Problem querying database'));
  }
);

// Accepts raw inputs from client
// Used during query editing
router.post('/api/datalog-result', mustBeAuthenticated, function(req, res) {
  const data = {
    cacheKey: req.body.cacheKey,
    config: req.config,
    connectionId: req.body.connectionId,
    timeout: req.body.timeout,
    verification: req.body.verification,
    optimization: req.body.optimization,
    speedup: req.body.speedup,
    datalogName: req.body.datalogName,
    datalogText: req.body.datalogText,
    user: req.user
  };

  return getDatalogResult(data)
    .then(datalogResult => res.send({ datalogResult }))
    .catch(error => sendError(res, error));
});

function getDatalogResult(data) {
  return connections
    .findOneById(data.connectionId)
    .then(connection => {
      // if (!connection) {
      //   throw new Error('Please choose a connection');
      // }
      if(connection) connection.maxRows = Number(data.config.get('queryResultMaxRows'));
      data.connection = connection;
      return Cache.findOneByCacheKey(data.cacheKey);
    })
    .then(cache => {
      if (!cache) {
        cache = new Cache({ cacheKey: data.cacheKey });
      }
      cache.queryName = sanitize(
        (data.datalogName || 'translated SQL') +
          ' ' +
          moment().format('YYYY-MM-DD')
      );
      // Expire cache in 8 hours
      const now = new Date();
      cache.expiration = new Date(now.getTime() + 1000 * 60 * 60 * 8);
      return cache.save();
    })
    .then(newCache => {
      data.cache = newCache;
      return runDatalog(data.datalogText, data.connection, data.user, data.timeout, data.verification, data.optimization, data.speedup).then(
        result => {
          data.datalogResult = result;
          data.datalogResult.cacheKey = data.cacheKey;
        }
      );
    })
    .then(() => {
      if (data.config.get('allowCsvDownload')) {
        const datalogResult = data.datalogResult;
        const cache = data.cache;
        return cache.writeSql(datalogResult);
      }
    })
    .then(() => {
      return data && data.datalogResult ? data.datalogResult : null;
    });
}

module.exports = router;
