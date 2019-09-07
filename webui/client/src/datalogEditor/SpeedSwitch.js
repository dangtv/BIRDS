import Switch from 'antd/lib/switch';
import Popover from 'antd/lib/popover';
import Icon from 'antd/lib/icon';
import React from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';

function SpeedSwitch({
  speedup,
  setSpeedup
}) {
  const handleChange = t => {
    setSpeedup(t);
  };

  // NOTE in order by placeholder to appear value must be set to undefined
  return (
    <>
      <Popover placement="bottom" content="Enable Speed-up" trigger="hover">
        <Switch
          checked={speedup}
          onChange={handleChange}
        />
      </Popover>
    </>
  );
}

export default connect(
  ['speedup'],
  actions
)(SpeedSwitch);
