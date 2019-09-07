import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import DatalogResultDataTable from '../common/DatalogResultDataTable.js';

const ConnectedDatalogEditorResult = connect(
  ['isRunning', 'datalogError', 'datalogResult'],
  actions
)(DatalogResultDataTable);

export default ConnectedDatalogEditorResult;
