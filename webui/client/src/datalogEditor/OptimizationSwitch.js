import Switch from 'antd/lib/switch';
import Popover from 'antd/lib/popover';
import Icon from 'antd/lib/icon';
import React from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';

function OptimizationSwitch({
  optimization,
  setOptimization
}) {
  const handleChange = t => {
    setOptimization(t);
  };

  // NOTE in order by placeholder to appear value must be set to undefined
  return (
    <>
      <Popover placement="bottom" content="Enable optimization" trigger="hover">
        <Switch
          checked={optimization}
          onChange={handleChange}
        />
      </Popover>
    </>
  );
}

export default connect(
  ['optimization'],
  actions
)(OptimizationSwitch);
