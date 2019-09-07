import PropTypes from 'prop-types';
import React from 'react';

function AboutContent({ version }) {
  return (
    <div>
      <p>
        <strong>Version</strong>: {version && version.current}
      </p>
      {version && version.updateAvailable && (
        <p>
          <strong>Update available</strong>: {version && version.latest}
        </p>
      )}
      <p>
        <strong>Project Page</strong>:{' '}
        <a
          href="https://dangtv.github.io/BIRDS/"
          target="_blank"
          rel="noopener noreferrer"
        >
          https://dangtv.github.io/BIRDS/{' '}
          <span
            style={{ marginLeft: 4 }}
            className="glyphicon glyphicon-new-window"
            aria-hidden="true"
          />
        </a>
      </p>
      <hr />
      <ul className="nav nav-pills nav-justified">
        <li role="presentation">
          <a
            href="https://github.com/dangtv/BIRDS/issues"
            target="_blank"
            rel="noopener noreferrer"
          >
            Submit an Issue{' '}
            <span
              className="glyphicon glyphicon-new-window"
              aria-hidden="true"
            />
          </a>
        </li>
        <li role="presentation">
          <a
            href="https://github.com/dangtv/BIRDS/"
            target="_blank"
            rel="noopener noreferrer"
          >
            GitHub Repository{' '}
            <span
              className="glyphicon glyphicon-new-window"
              aria-hidden="true"
            />
          </a>
        </li>
      </ul>
      <hr />
      <p>
        Powered by {' '}
        <a
          href="http://rickbergfalk.github.io/sqlpad/"
          target="_blank"
          rel="noopener noreferrer"
        >
          SQLPad{' '}
          <span
            style={{ marginLeft: 4 }}
            className="glyphicon glyphicon-new-window"
            aria-hidden="true"
          />
        </a>
      </p>
    </div>
  );
}

AboutContent.propTypes = {
  version: PropTypes.shape({
    current: PropTypes.string,
    latest: PropTypes.string,
    updateAvailable: PropTypes.bool
  })
};

AboutContent.defaultProps = {
  version: {}
};

export default AboutContent;
