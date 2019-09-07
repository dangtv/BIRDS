import React from 'react';
import FullscreenMessage from './common/FullscreenMessage.js';

export default props => {
  document.title = 'BIRDS Editor - Password Reset';
  return (
    <FullscreenMessage>
      <p>Password reset requested.</p>
      <p>An email has been sent with further instruction.</p>
    </FullscreenMessage>
  );
};
