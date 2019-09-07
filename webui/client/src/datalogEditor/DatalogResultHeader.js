import React from 'react';
import PropTypes from 'prop-types';
import { Link } from 'react-router-dom';
import IncompleteDataNotification from '../common/IncompleteDataNotification';
import SecondsTimer from '../common/SecondsTimer.js';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';

function DatalogResultHeader({
  cacheKey,
  config,
  isRunning,
  datalogResult,
  runDatalogStartTime
}) {
  if (isRunning || !datalogResult) {
    return (
      <div
        className="bb b--moon-gray bg-near-white pa2 nowrap fw6 near-black"
        style={{ height: '30px' }}
      >
        {isRunning ? (
          <span className="pl1 pr5">
            <span className="gray">Datalog Run Time: </span>
            <span>
              <SecondsTimer startTime={runDatalogStartTime} /> sec.
            </span>
          </span>
        ) : null}
      </div>
    );
  }

  const serverSec = datalogResult
    ? datalogResult.datalogRunTime / 1000 + ' sec.'
    : '';
  const rowCount = 1; //datalogResult && datalogResult.rows ? datalogResult.rows.length : '';

  const incomplete = datalogResult ? datalogResult.incomplete : false;

  const sqlDownloadLink = `/download-results/${cacheKey}.sql`;
  // const csvDownloadLink = `/download-results/${cacheKey}.csv`;
  // const xlsxDownloadLink = `/download-results/${cacheKey}.xlsx`;

  return (
    <div
      className="bb b--moon-gray bg-near-white pa2 nowrap fw6 near-black"
      style={{ height: '30px' }}
    >
      <span className="pl1 pr5">
        <span className="gray">Datalog Run Time: </span>
        {serverSec}
      </span>
      {/* <span className="pr5">
        <span className="gray">Rows: </span>
        {rowCount}
      </span> */}
      <span className="pr5">
        {config.allowCsvDownload && (
          <span>
            <span className="gray">Download: </span>
            <Link
              className="ml3"
              target="_blank"
              rel="noopener noreferrer"
              to={sqlDownloadLink}
            >
              .sql
            </Link>
          </span>
        )}
      </span>
      <span className="pr5">
        <IncompleteDataNotification incomplete={incomplete} />
      </span>
    </div>
  );
}

DatalogResultHeader.propTypes = {
  cacheKey: PropTypes.string,
  config: PropTypes.object,
  isRunning: PropTypes.bool,
  datalogResult: PropTypes.object,
  runDatalogStartTime: PropTypes.instanceOf(Date)
};

DatalogResultHeader.defaultProps = {
  cacheKey: '',
  config: {},
  isRunning: false
};

export default connect(
  ['cacheKey', 'config', 'isRunning', 'datalogResult', 'runDatalogStartTime'],
  actions
)(React.memo(DatalogResultHeader));
