import Button from 'antd/lib/button';
import Form from 'antd/lib/form';
import Input from 'antd/lib/input';
import Tooltip from 'antd/lib/tooltip';
import Badge from 'antd/lib/badge';
import Icon from 'antd/lib/icon';
import { connect } from 'unistore/react';
import { actions } from '../../stores/unistoreStore';
import PropTypes from 'prop-types';
import React, { useState } from 'react';
import ConnectionDropDown from '../ConnectionDropdown';
import TimeoutDropDown from '../TimeoutDropdown';
import VerificationSwitch from '../VerificationSwitch';
import OptimizationSwitch from '../OptimizationSwitch';
import SpeedSwitch from '../SpeedSwitch';
import AboutButton from './AboutButton';
import SignoutButton from './SignoutButton';
import ConfigButton from './ConfigButton';
import DatalogListButton from './DatalogListButton';
import DatalogDetailsModal from './DatalogDetailsModal';
import IconButtonLink from '../../common/IconButtonLink';
import Popover from 'antd/lib/popover';

const FormItem = Form.Item;

function mapStateToProps(state) {
  return {
    currentUser: state.currentUser,
    isRunning: state.isRunning,
    isSaving: state.isSaving,
    datalogId: state.datalog && state.datalog._id,
    datalogName: state.datalog && state.datalog.name,
    showValidation: state.showValidation,
    unsavedChanges: state.unsavedChanges
  };
}

const ConnectedEditorNavBar = connect(
  mapStateToProps,
  actions
)(React.memo(Toolbar));

function Toolbar({
  currentUser,
  formatDatalog,
  handleCloneClickDatalog,
  isRunning,
  isSaving,
  datalogId,
  datalogName,
  resetNewDatalog,
  importSchemaDatalog,
  runDatalog,
  saveDatalog,
  setDatalogState,
  showValidation,
  toggleSchema,
  toggleVisSidebar,
  unsavedChanges
}) {
  const [showDetails, setShowDetails] = useState(false);

  const validationState =
    showValidation && !datalogName.length ? 'error' : null;
  const cloneDisabled = !datalogId;

  const isAdmin = currentUser.role === 'admin';

  return (
    <div className="w-100 bg-near-white ph2 pv1 bb b--light-gray">
      <Form className="flex" layout="inline">
        <FormItem>
          <Button.Group>
            <Tooltip placement="bottom" title="Show Schema">
              <Button icon="database" onClick={toggleSchema} />
            </Tooltip>
            <Tooltip placement="bottom" title="All Programs">
              <DatalogListButton />
            </Tooltip>
            {/* <Button icon="bar-chart" onClick={toggleVisSidebar} /> */}
          </Button.Group>
          <ConnectionDropDown />
        </FormItem>

        <FormItem validateStatus={validationState}>
          <Tooltip placement="bottom" title="New datalog">
            <IconButtonLink to="/datalog/new" onClick={() => resetNewDatalog()}>
              <Icon type="plus" />
            </IconButtonLink>
          </Tooltip>
          <Tooltip placement="bottom" title="Import database schema">
            <IconButtonLink
              to="/datalog/new"
              onClick={() => importSchemaDatalog()}
            >
              <Icon type="import" />
            </IconButtonLink>
          </Tooltip>
          <Input
            className="w5"
            placeholder="Datalog name"
            style={{ width: 200 }}
            value={datalogName}
            onChange={e => setDatalogState('name', e.target.value)}
            addonAfter={
              <Tooltip placement="bottom" title="Tags">
                <Icon onClick={() => setShowDetails(true)} type="tags" />
                <DatalogDetailsModal
                  visible={showDetails}
                  onClose={() => setShowDetails(false)}
                />
              </Tooltip>
            }
          />
          <Button.Group>
            <Tooltip placement="bottom" title="Clone">
              <Button
                onClick={handleCloneClickDatalog}
                disabled={cloneDisabled}
              >
                <Icon type="copy" />
              </Button>
            </Tooltip>
            {/* <Tooltip placement="bottom" title="Format">
              <Button onClick={formatDatalog}>
                <Icon type="align-left" />
              </Button>
            </Tooltip> */}

            <Tooltip placement="bottom" title="Save">
              <Button onClick={() => saveDatalog()} disabled={isSaving}>
                <Badge dot={unsavedChanges}>
                  <Icon type="save" />
                </Badge>
              </Button>
            </Tooltip>
          </Button.Group>
        </FormItem>

        <FormItem>
          Verify &nbsp;
          <VerificationSwitch />
          &nbsp; Optimize &nbsp;
          <OptimizationSwitch />
          &nbsp; Speed &nbsp;
          <SpeedSwitch />
          &nbsp;
          {/* Timeout &nbsp; */}
          <Popover placement="bottom" content="Timeout" trigger="hover">
            {/* <Icon type="clock-circle" /> &nbsp; */}
            <TimeoutDropDown />
          </Popover>
          &nbsp;
          <Tooltip placement="bottom" title="Compile (with/without verification) into SQL">
            <Button
              type="primary"
              onClick={() => runDatalog(0, false)}
              disabled={isRunning}
            >
              Compile
            </Button>
          </Tooltip>
          &nbsp;
          <Tooltip placement="bottom" title="Test for the given tuples">
            <Button
              type="danger"
              onClick={() => runDatalog(0, true)}
              disabled={isRunning}
            >
              Test
            </Button>
          </Tooltip>
        </FormItem>

        <div className="flex-grow-1" />

        <FormItem>
          <Tooltip placement="bottom" title="Go to SQL tool">
            <IconButtonLink to="/queries/new">
              <Icon type="edit" /> SQL
            </IconButtonLink>
          </Tooltip>
          <Button.Group>
            <AboutButton />
            {isAdmin && <ConfigButton />}
            <SignoutButton />
          </Button.Group>
        </FormItem>
      </Form>
    </div>
  );
}

Toolbar.propTypes = {
  isSaving: PropTypes.bool.isRequired,
  isRunning: PropTypes.bool.isRequired,
  handleCloneClickDatalog: PropTypes.func.isRequired,
  saveDatalog: PropTypes.func.isRequired,
  runDatalog: PropTypes.func.isRequired,
  formatDatalog: PropTypes.func.isRequired,
  datalogName: PropTypes.string.isRequired,
  datalogId: PropTypes.string,
  showValidation: PropTypes.bool.isRequired,
  unsavedChanges: PropTypes.bool.isRequired
};

export default ConnectedEditorNavBar;
