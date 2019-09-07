import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import SqlpadTauChart from '../common/SqlpadTauChart';

function mapStateToProps(state) {
  return {
    datalogId: (state.datalog && state.datalog._id) || 'new',
    isRunning: state.isRunning,
    datalogError: state.datalogError,
    datalogResult: state.datalogResult,
    chartConfiguration: state.datalog && state.datalog.chartConfiguration,
    datalogName: state.datalog && state.datalog.name
  };
}

const ConnectedChart = connect(
  mapStateToProps,
  actions
)(SqlpadTauChart);

export default ConnectedChart;
