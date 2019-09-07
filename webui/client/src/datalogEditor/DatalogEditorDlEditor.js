import React, { useCallback } from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import DlEditor from '../common/DlEditor';

function mapStateToProps(state, props) {
  return {
    value: state.datalog && state.datalog.datalogText
  };
}

function DatalogEditorDlEditor({
  value,
  setDatalogState,
  handleDatalogSelectionChange
}) {
  const onChange = useCallback(value => setDatalogState('datalogText', value), [
    setDatalogState
  ]);

  return (
    <DlEditor
      value={value}
      onChange={onChange}
      onSelectionChange={handleDatalogSelectionChange}
      fontSize="20pt"
    />
  );
}

const ConnectedDatalogEditorDlEditor = connect(
  mapStateToProps,
  actions
)(DatalogEditorDlEditor);

export default ConnectedDatalogEditorDlEditor;
