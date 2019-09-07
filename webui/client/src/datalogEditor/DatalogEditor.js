import keymaster from 'keymaster';
import PropTypes from 'prop-types';
import Layout from 'antd/lib/layout';
import React from 'react';
import SplitPane from 'react-split-pane';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import DatalogEditorResult from './DatalogEditorResult';
import DatalogEditorDlEditor from './DatalogEditorDlEditor';
import DatalogEditorChart from './DatalogEditorChart';
import Toolbar from './toolbar/Toolbar';

import DatalogResultHeader from './DatalogResultHeader.js';
import SchemaSidebar from '../schema/SchemaSidebar.js';
import VisSidebar from './VisSidebar';
import { resizeChart } from '../common/tauChartRef';

const { Content } = Layout;

// TODO FIXME XXX capture unsaved state to local storage
// Prompt is removed. It doesn't always work anyways

class DatalogEditor extends React.Component {
  componentDidUpdate(prevProps) {
    const { datalogId, resetNewDatalog, loadDatalog } = this.props;
    if (datalogId !== prevProps.datalogId) {
      if (datalogId === 'new') {
        return resetNewDatalog();
      }
      return loadDatalog(datalogId);
    }
  }

  async componentDidMount() {
    const {
      datalogId,
      loadConnections,
      loadTags,
      loadDatalog,
      saveDatalog,
      runDatalog,
      // formatDatalog,
      resetNewDatalog
    } = this.props;

    await Promise.all([loadConnections(), loadTags()]);
    if (datalogId !== 'new') {
      await loadDatalog(datalogId);
    } else {
      // TODO FIXME XXX this won't reset datalog state from new to new
      resetNewDatalog();
    }

    /*  Shortcuts
    ============================================================================== */
    // keymaster doesn't fire on input/textarea events by default
    // since we are only using command/ctrl shortcuts,
    // we want the event to fire all the time for any element
    keymaster.filter = () => true;
    keymaster('ctrl+s, command+s', e => {
      saveDatalog();
      return false;
    });
    keymaster('ctrl+return, command+return', e => {
      runDatalog();
      return false;
    });
    // keymaster('shift+return', e => {
    //   formatDatalog();
    //   return false;
    // });
  }

  componentWillUnmount() {
    keymaster.unbind('ctrl+return, command+return');
    keymaster.unbind('ctrl+s, command+s');
    keymaster.unbind('shift+return');
  }

  handleVisPaneResize = () => {
    const { datalogId } = this.props;
    resizeChart(datalogId);
  };

  render() {
    const {
      chartType,
      datalogName,
      showSchema,
      showVisSidebar,
      datalogId
    } = this.props;

    document.title = datalogName;

    const editorAndVis = chartType ? (
      <SplitPane
        key="editorAndVis"
        split="vertical"
        defaultSize={'50%'}
        maxSize={-200}
        onChange={this.handleVisPaneResize}
      >
        <DatalogEditorDlEditor />
        <div className="flex-auto h-100">
          <DatalogEditorChart />
        </div>
      </SplitPane>
    ) : (
      <DatalogEditorDlEditor />
    );

    const editorResultPane = (
      <SplitPane
        split="horizontal"
        minSize={100}
        defaultSize={'60%'}
        maxSize={-100}
        onChange={this.handleVisPaneResize}
      >
        {editorAndVis}
        <div>
          <DatalogResultHeader />
          <div
            style={{
              position: 'absolute',
              top: 30,
              bottom: 0,
              left: 0,
              right: 0
            }}
          >
            <DatalogEditorResult />
          </div>
        </div>
      </SplitPane>
    );

    let sidebar = null;
    if (showSchema) {
      sidebar = <SchemaSidebar />;
    } else if (showVisSidebar) {
      sidebar = <VisSidebar datalogId={datalogId} />;
    }

    const sqlTabPane = sidebar ? (
      <SplitPane
        split="vertical"
        minSize={150}
        defaultSize={280}
        maxSize={-100}
        onChange={this.handleVisPaneResize}
      >
        {sidebar}
        {editorResultPane}
      </SplitPane>
    ) : (
      editorResultPane
    );

    return (
      <Layout style={{ minHeight: '100vh' }} className="flex w-100 bg-white">
        <Content className="flex w-100">
          <div className="flex w-100" style={{ flexDirection: 'column' }}>
            <Toolbar />
            <div style={{ position: 'relative', flexGrow: 1 }}>
              {sqlTabPane}
            </div>
          </div>
        </Content>
      </Layout>
    );
  }
}

DatalogEditor.propTypes = {
  formatDatalog: PropTypes.func.isRequired,
  loadConnections: PropTypes.func.isRequired,
  loadDatalog: PropTypes.func.isRequired,
  loadTags: PropTypes.func.isRequired,
  datalogId: PropTypes.string.isRequired,
  datalogName: PropTypes.string,
  resetNewDatalog: PropTypes.func.isRequired,
  runDatalog: PropTypes.func.isRequired,
  saveDatalog: PropTypes.func.isRequired
};

DatalogEditor.defaultProps = {
  datalogName: 'New datalog'
};

function mapStateToProps(state, props) {
  return {
    chartType:
      state.datalog &&
      state.datalog.chartConfiguration &&
      state.datalog.chartConfiguration.chartType,
    datalogName: state.datalog && state.datalog.name,
    showSchema: state.showSchema,
    showVisSidebar: state.showVisSidebar
  };
}

export default connect(
  mapStateToProps,
  actions
)(DatalogEditor);
