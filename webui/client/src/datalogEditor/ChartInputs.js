import Checkbox from 'antd/lib/checkbox';
import Input from 'antd/lib/input';
import Select from 'antd/lib/select';
import PropTypes from 'prop-types';
import React, { useState } from 'react';
import chartDefinitions from '../utilities/chartDefinitions.js';

const { Option } = Select;

function cleanBoolean(value) {
  if (typeof value === 'string') {
    if (value.toLowerCase() === 'true') {
      value = true;
    } else if (value.toLowerCase() === 'false') {
      value = false;
    }
  }
  return value;
}

const inputClassName = 'mt3 mb3';

function ChartInputs({
  onChartConfigurationFieldsChange,
  datalogChartConfigurationFields,
  datalogResult,
  chartType
}) {
  const [showAdvanced, setShowAdvanced] = useState(false);

  const handleAdvancedClick = e => {
    e.preventDefault();
    setShowAdvanced(!showAdvanced);
  };

  const changeChartConfigurationField = (chartFieldId, datalogResultField) => {
    onChartConfigurationFieldsChange(chartFieldId, datalogResultField);
  };

  const renderFormGroup = inputDefinitionFields => {
    const datalogResultFields = datalogResult.fields || [];

    return inputDefinitionFields.map(field => {
      if (field.inputType === 'field-dropdown') {
        const optionNodes = datalogResultFields.map(qrfield => {
          return (
            <Option key={qrfield} value={qrfield}>
              {qrfield}
            </Option>
          );
        });
        const selectedDatalogResultField =
          datalogChartConfigurationFields[field.fieldId];
        if (
          selectedDatalogResultField &&
          datalogResultFields.indexOf(selectedDatalogResultField) === -1
        ) {
          optionNodes.push(
            <Option
              key={'selectedDatalogResultField'}
              value={selectedDatalogResultField}
            >
              {selectedDatalogResultField}
            </Option>
          );
        }
        return (
          <div className={inputClassName} key={field.fieldId}>
            <label>{field.label}</label>
            <Select
              allowClear
              showSearch
              className="w-100"
              optionFilterProp="children"
              value={selectedDatalogResultField}
              notFoundContent="No fields available"
              onChange={value =>
                changeChartConfigurationField(field.fieldId, value)
              }
              filterOption={(input, option) =>
                option.props.value &&
                option.props.children
                  .toLowerCase()
                  .indexOf(input.toLowerCase()) >= 0
              }
            >
              {optionNodes}
            </Select>
          </div>
        );
      } else if (field.inputType === 'checkbox') {
        const checked =
          cleanBoolean(datalogChartConfigurationFields[field.fieldId]) || false;
        return (
          <div className={inputClassName} key={field.fieldId}>
            <Checkbox
              checked={checked}
              name={field.key}
              onChange={e =>
                changeChartConfigurationField(field.fieldId, e.target.checked)
              }
            >
              {field.label}
            </Checkbox>
          </div>
        );
      } else if (field.inputType === 'textbox') {
        const value = datalogChartConfigurationFields[field.fieldId] || '';
        return (
          <div className={inputClassName} key={field.fieldId}>
            <label>{field.label}</label>
            <Input
              value={value}
              onChange={e =>
                changeChartConfigurationField(field.fieldId, e.target.value)
              }
              className="w-100"
            />
          </div>
        );
      } else {
        throw Error(`field.inputType ${field.inputType} not supported`);
      }
    });
  };

  const chartDefinition = chartDefinitions.find(
    def => def.chartType === chartType
  );

  if (!chartDefinition || !chartDefinition.fields) {
    return null;
  }

  const regularFields = chartDefinition.fields.filter(
    field => field.advanced == null || field.advanced === false
  );

  const advancedFields = chartDefinition.fields.filter(
    field => field.advanced === true
  );

  const advancedLink = advancedFields.length ? (
    <a href="#settings" onClick={handleAdvancedClick}>
      {showAdvanced ? 'hide advanced settings' : 'show advanced settings'}
    </a>
  ) : null;

  return (
    <div>
      {renderFormGroup(regularFields)}
      {advancedLink}
      {showAdvanced && renderFormGroup(advancedFields)}
    </div>
  );
}

ChartInputs.propTypes = {
  chartType: PropTypes.string,
  onChartConfigurationFieldsChange: PropTypes.func.isRequired,
  datalogChartConfigurationFields: PropTypes.object,
  datalogResult: PropTypes.object
};

ChartInputs.defaultProps = {
  datalogChartConfigurationFields: {},
  datalogResult: {}
};

export default ChartInputs;
