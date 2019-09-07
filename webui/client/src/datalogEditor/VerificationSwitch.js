import Switch from 'antd/lib/switch';
import Popover from 'antd/lib/popover';
import Icon from 'antd/lib/icon';
import React from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';

function VerificationSwitch({
  verification,
  setVerification
}) {
  const handleChange = t => {
    setVerification(t);
  };

  // NOTE in order by placeholder to appear value must be set to undefined
  return (
    <>
      <Popover placement="bottom" content="Enable verification" trigger="hover">
        <Switch
          checked={verification}
          onChange={handleChange}
        />
      </Popover>
    </>
  );
}

export default connect(
  ['verification'],
  actions
)(VerificationSwitch);
