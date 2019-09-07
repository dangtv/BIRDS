import Icon from 'antd/lib/icon';
import Modal from 'antd/lib/modal';
import Tooltip from 'antd/lib/tooltip';
import React from 'react';
import EditableTagGroup from '../../common/EditableTagGroup';
import { Link } from 'react-router-dom';
import { connect } from 'unistore/react';
import { actions } from '../../stores/unistoreStore';

function mapStateToProps(state) {
  return {
    availableTags: state.availableTags,
    datalogId: state.datalog && state.datalog._id,
    datalogTags: state.datalog && state.datalog.tags
  };
}

const ConnectedDatalogDetailsModal = connect(
  mapStateToProps,
  actions
)(React.memo(DatalogDetailsModal));

function DatalogDetailsModal({
  datalogId,
  datalogTags,
  visible,
  setDatalogState,
  availableTags,
  onClose
}) {
  const tagOptions = availableTags.slice();
  if (datalogTags) {
    datalogTags.forEach(t => {
      if (tagOptions.indexOf(t) === -1) {
        tagOptions.push(t);
      }
    });
  }

  const renderNavLink = (href, text) => {
    const saved = !!datalogId;
    if (saved) {
      return (
        <li role="presentation">
          <Link to={href} target="_blank" rel="noopener noreferrer">
            {text} <Icon type="export" />
          </Link>
        </li>
      );
    } else {
      return (
        <Tooltip title="Save datalog to enable table/chart view links">
          <li role="presentation" className="disabled">
            <Link
              to={href}
              target="_blank"
              rel="noopener noreferrer"
              onClick={e => e.preventDefault()}
            >
              {text} <Icon type="export" />
            </Link>
          </li>
        </Tooltip>
      );
    }
  };

  const tableUrl = `/datalog-table/${datalogId}`;
  const chartUrl = `/datalog-chart/${datalogId}`;

  return (
    <Modal
      width={'600px'}
      visible={visible}
      cancelText={null}
      onCancel={onClose}
      footer={null}
    >
      <label>Datalog Tags</label>
      <EditableTagGroup
        tags={datalogTags}
        onChange={values => setDatalogState('tags', values)}
        tagOptions={tagOptions}
      />
      <hr />
      <p>
        <label>Shortcuts</label>
      </p>
      <ul style={{ paddingLeft: 0 }}>
        <li style={{ listStyleType: 'none', marginBottom: 8 }}>
          <code>ctrl+s</code> / <code>command+s</code> : Save
        </li>
        <li style={{ listStyleType: 'none', marginBottom: 8 }}>
          <code>ctrl+return</code> / <code>command+return</code> : Run
        </li>
        <li style={{ listStyleType: 'none', marginBottom: 8 }}>
          <code>shift+return</code> : Format
        </li>
      </ul>
      <hr />
      <p>
        <strong>Tip</strong>
      </p>
      <p>Run only a portion of a datalog by highlighting it first.</p>
      <hr />
      <ul className="nav nav-pills nav-justified">
        {renderNavLink(tableUrl, 'Link to Table')}
        {renderNavLink(chartUrl, 'Link to Chart')}
      </ul>
    </Modal>
  );
}

export default ConnectedDatalogDetailsModal;
