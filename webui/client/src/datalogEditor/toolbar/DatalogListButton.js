import Button from 'antd/lib/button';
import Drawer from '../../common/Drawer';
import React, { useState } from 'react';
import DatalogList from '../../datalog/DatalogList';

function DatalogListButton() {
  const [showDatalogs, setShowDatalogs] = useState(false);

  return (
    <>
      <Button icon="file-text" onClick={() => setShowDatalogs(true)}>
        Datalog
      </Button>
      <Drawer
        title={'Datalog programs'}
        visible={showDatalogs}
        width="600"
        onClose={() => setShowDatalogs(false)}
        placement="left"
      >
        <DatalogList onSelect={() => setShowDatalogs(false)} />
      </Drawer>
    </>
  );
}

export default React.memo(DatalogListButton);
