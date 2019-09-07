import Button from 'antd/lib/button';
import Icon from 'antd/lib/icon';
import Select from 'antd/lib/select';
import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import Sidebar from '../common/Sidebar';
import SidebarBody from '../common/SidebarBody';
import chartDefinitions from '../utilities/chartDefinitions.js';
import ChartInputs from './ChartInputs.js';
import { exportPng } from '../common/tauChartRef';
const { Option } = Select;

function mapStateToProps(state) {
  return {
    datalogResult: state.datalogResult,
    chartType:
      state.datalog &&
      state.datalog.chartConfiguration &&
      state.datalog.chartConfiguration.chartType,
    fields:
      state.datalog &&
      state.datalog.chartConfiguration &&
      state.datalog.chartConfiguration.fields
  };
}

const ConnectedVisSidebar = connect(
  mapStateToProps,
  actions
)(React.memo(VisSidebar));

function VisSidebar({
  chartType,
  fields,
  datalogResult,
  handleChartTypeChange,
  handleChartConfigurationFieldsChange,
  datalogId
}) {
  const chartOptions = chartDefinitions.map(d => {
    return (
      <Option key={d.chartType} value={d.chartType}>
        {d.chartLabel}
      </Option>
    );
  });

  return (
    <Sidebar>
      <SidebarBody>
        <Select
          allowClear
          showSearch
          className="w-100"
          optionFilterProp="children"
          value={chartType}
          notFoundContent="No charts available"
          onChange={handleChartTypeChange}
          filterOption={(input, option) =>
            option.props.value &&
            option.props.children.toLowerCase().indexOf(input.toLowerCase()) >=
              0
          }
        >
          {chartOptions}
        </Select>
        <ChartInputs
          chartType={chartType}
          datalogChartConfigurationFields={fields}
          onChartConfigurationFieldsChange={
            handleChartConfigurationFieldsChange
          }
          datalogResult={datalogResult}
        />
      </SidebarBody>
      <div className="pa2 bt b--near-white">
        <Button className="w-100 mb1" onClick={() => exportPng(datalogId)}>
          <Icon type="download" /> Save Chart Image
        </Button>
      </div>
    </Sidebar>
  );
}

VisSidebar.propTypes = {
  onChartConfigurationFieldsChange: PropTypes.func,
  onChartTypeChange: PropTypes.func,
  onSaveImageClick: PropTypes.func,
  datalog: PropTypes.object,
  datalogId: PropTypes.string,
  datalogResult: PropTypes.object
};

export default ConnectedVisSidebar;
