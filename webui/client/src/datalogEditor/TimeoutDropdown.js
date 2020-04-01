import Select from 'antd/lib/select';
import Popover from 'antd/lib/popover';
import Icon from 'antd/lib/icon';
import React from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';

const { Option } = Select;

function TimeoutDropdown({ timeout, selectTimeout }) {
  const handleChange = t => {
    selectTimeout(t);
  };

  // NOTE in order by placeholder to appear value must be set to undefined
  return (
    <>
      <Select
        showSearch
        placeholder="Timeout"
        style={{ width: 100 }}
        optionFilterProp="children"
        value={timeout.toString() || undefined}
        // defaultValue={"1m" || undefined}
        onChange={handleChange}
        filterOption={(input, option) =>
          option.props.value &&
          option.props.name &&
          option.props.name.toLowerCase().indexOf(input.toLowerCase()) >= 0
        }
      >
        <Option style={{ borderTop: '1px solid #ccc' }} value="5" name="5s">
          <Icon type="clock-circle" /> <em>5s</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="10" name="10s">
          <Icon type="clock-circle" /> <em>10s</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="30" name="30s">
          <Icon type="clock-circle" /> <em>30s</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="60" name="1m">
          <Icon type="clock-circle" /> <em>1m</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="120" name="2m">
          <Icon type="clock-circle" /> <em>2m</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="300" name="5m">
          <Icon type="clock-circle" /> <em>5m</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="600" name="10m">
          <Icon type="clock-circle" /> <em>10m</em>
        </Option>

        <Option style={{ borderTop: '1px solid #ccc' }} value="3600" name="1h">
          <Icon type="clock-circle" /> <em>1h</em>
        </Option>
      </Select>
    </>
  );
}

export default connect(
  ['timeout'],
  actions
)(TimeoutDropdown);
