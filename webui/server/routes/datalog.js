const router = require('express').Router();
const Datalog = require('../models/Datalog.js');
const mustBeAuthenticated = require('../middleware/must-be-authenticated.js');
const mustBeAuthenticatedOrChartLink = require('../middleware/must-be-authenticated-or-chart-link-noauth.js');
const sendError = require('../lib/sendError');

/*  render page routes
============================================================================= */

// NOTE: this non-api route is special since it redirects legacy urls
router.get('/datalog/:_id', mustBeAuthenticatedOrChartLink, function(
  req,
  res,
  next
) {
  const { config, query, params } = req;
  const { format } = query;
  if (format === 'table') {
    return res.redirect(config.get('baseUrl') + '/query-table/' + params._id);
  } else if (format === 'chart') {
    return res.redirect(config.get('baseUrl') + '/query-chart/' + params._id);
  }
  next();
});

/*  API routes
============================================================================= */

router.delete('/api/datalog/:_id', mustBeAuthenticated, function(req, res) {
  return Datalog.removeOneById(req.params._id)
    .then(() => res.json({}))
    .catch(error => sendError(res, error, 'Problem deleting datalog'));
});

router.get('/api/datalog', mustBeAuthenticated, function(req, res) {
  return Datalog.findAll()
    .then(datalogs => res.json({ datalogs }))
    .catch(error => sendError(res, error, 'Problem querying datalog database'));
});

router.get('/api/datalog/:_id', mustBeAuthenticatedOrChartLink, function(
  req,
  res
) {
  return Datalog.findOneById(req.params._id)
    .then(datalog => {
      if (!datalog) {
        return res.json({
          datalog: {}
        });
      }
      return res.json({ datalog });
    })
    .catch(error => sendError(res, error, 'Problem getting datalog'));
});

// create new
router.post('/api/datalog', mustBeAuthenticated, function(req, res) {
  const datalog = new Datalog({
    name: req.body.name || 'No Name Datalog',
    tags: req.body.tags,
    connectionId: req.body.connectionId,
    datalogText: req.body.datalogText,
    chartConfiguration: req.body.chartConfiguration,
    createdBy: req.user.email,
    modifiedBy: req.user.email
  });
  return datalog
    .save()
    .then(newDatalog => {
      // This is async, but save operation doesn't care about when/if finished
      newDatalog.pushDatalogToSlackIfSetup();
      return res.json({
        datalog: newDatalog
      });
    })
    .catch(error => sendError(res, error, 'Problem saving datalog'));
});

router.put('/api/datalog/:_id', mustBeAuthenticated, function(req, res) {
  return Datalog.findOneById(req.params._id)
    .then(datalog => {
      if (!datalog) {
        return sendError(res, null, 'Datalog not found');
      }

      datalog.name = req.body.name || '';
      datalog.tags = req.body.tags;
      datalog.connectionId = req.body.connectionId;
      datalog.datalogText = req.body.datalogText;
      datalog.chartConfiguration = req.body.chartConfiguration;
      datalog.modifiedBy = req.user.email;

      return datalog.save().then(newDatalog => res.json({ datalog: newDatalog }));
    })
    .catch(error => sendError(res, error, 'Problem saving datalog'));
});

module.exports = router;
