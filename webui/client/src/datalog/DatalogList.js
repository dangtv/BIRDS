import Button from 'antd/lib/button';
import Icon from 'antd/lib/icon';
import List from 'antd/lib/list';
import Row from 'antd/lib/row';
import Col from 'antd/lib/col';
import Select from 'antd/lib/select';
import Tooltip from 'antd/lib/tooltip';
import Typography from 'antd/lib/typography';
import Tag from 'antd/lib/tag';
import Divider from 'antd/lib/divider';
import PropTypes from 'prop-types';
import React, { useEffect, useState } from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import Popconfirm from 'antd/lib/popconfirm';
import getAvailableSearchTags from './getAvailableSearchTags';
import getDecoratedDatalogs from './getDecoratedDatalogs';
import IconButtonLink from '../common/IconButtonLink';
import DlEditor from '../common/DlEditor';

const { Option } = Select;
const { Title } = Typography;

function DatalogList({
  datalogs,
  loadDatalogs,
  connections,
  deleteDatalog,
  onSelect
}) {
  const [preview, setPreview] = useState('');
  const [searches, setSearches] = useState([]);
  useEffect(() => {
    loadDatalogs();
  }, [loadDatalogs]);

  const availableSearches = getAvailableSearchTags(datalogs, connections);
  const decoratedDatalogs = getDecoratedDatalogs(datalogs, connections);

  let filteredDatalogs = decoratedDatalogs;
  if (searches && searches.length) {
    searches.forEach(search => {
      if (search.startsWith('createdBy=')) {
        const createdBy = search.substring(10);
        filteredDatalogs = filteredDatalogs.filter(
          datalog => datalog.createdBy === createdBy
        );
      } else if (search.startsWith('tag=')) {
        const sTag = search.substring(4);
        filteredDatalogs = filteredDatalogs.filter(
          datalog => datalog.tags && datalog.tags.includes(sTag)
        );
      } else if (search.startsWith('connection=')) {
        const connectionName = search.substring(11);
        filteredDatalogs = filteredDatalogs.filter(
          datalog => datalog.connectionName === connectionName
        );
      } else {
        // search is just open text search
        const lowerSearch = search.toLowerCase();
        filteredDatalogs = filteredDatalogs.filter(q => {
          return (
            (q.name && q.name.toLowerCase().search(lowerSearch) !== -1) ||
            (q.datalogText &&
              q.datalogText.toLowerCase().search(lowerSearch) !== -1)
          );
        });
      }
    });
  }

  const renderItem = datalog => {
    const tableUrl = `/query-table/${datalog._id}`;
    const chartUrl = `/query-chart/${datalog._id}`;
    const datalogUrl = `/datalog/${datalog._id}`;

    return (
      <List.Item
        className="bg-animate hover-bg-near-white"
        onMouseEnter={() => setPreview(datalog)}
        onMouseLeave={() => setPreview('')}
        actions={[
          <Tooltip key="edit" title="Edit datalog">
            <IconButtonLink
              to={datalogUrl}
              onClick={() => {
                onSelect(datalog);
              }}
            >
              <Icon type="edit" />
            </IconButtonLink>
          </Tooltip>,
          <Tooltip key="table" title="Open results in new window">
            <IconButtonLink
              to={tableUrl}
              target="_blank"
              rel="noopener noreferrer"
            >
              <Icon type="table" />
            </IconButtonLink>
          </Tooltip>,
          <Tooltip key="chart" title="Open chart in new window">
            <IconButtonLink
              to={chartUrl}
              target="_blank"
              rel="noopener noreferrer"
            >
              <Icon type="bar-chart" />
            </IconButtonLink>
          </Tooltip>,
          <Popconfirm
            key="del"
            title="Are you sure?"
            onConfirm={e => deleteDatalog(datalog._id)}
            onCancel={() => {}}
            okText="Yes"
            cancelText="No"
          >
            <Button icon="delete" type="danger" />
          </Popconfirm>
        ]}
      >
        <List.Item.Meta title={datalog.name} description={datalog.connectionName} />
      </List.Item>
    );
  };

  return (
    <>
      <Row>
        <Col className="pb3" span={24}>
          <Select
            autoFocus
            className="w-100"
            mode="tags"
            placeholder="Search"
            value={searches}
            onChange={value => setSearches(value)}
          >
            {availableSearches.map(search => (
              <Option key={search}>{search}</Option>
            ))}
          </Select>
        </Col>
      </Row>
      <List
        size="small"
        itemLayout="horizontal"
        dataSource={filteredDatalogs}
        renderItem={renderItem}
      />
      {preview && (
        <div
          className="shadow-2 pa3"
          style={{
            position: 'fixed',
            left: 640,
            top: 40,
            right: 40,
            bottom: 40,
            backgroundColor: 'white',
            display: 'flex',
            flexDirection: 'column'
          }}
        >
          <Title level={4}>
            <Typography.Text>{preview.name}</Typography.Text>
          </Title>
          <Typography.Text>Connection {preview.connectionName}</Typography.Text>
          <Typography.Text>By {preview.createdBy}</Typography.Text>
          <div>
            {preview.tags &&
              preview.tags.map(tag => <Tag key={tag}>{tag}</Tag>)}
          </div>
          {/* needs to be wrapped in div because of flex height weirdness */}
          <div>
            <Divider />
          </div>
          {/* 
            This style necessary to get proper sizing on SqlEditor.
            It has height 100%, which looks to height of nearest containing BLOCK,
            which apparently looks past this flex container. This causes weirdness
          */}
          <div
            style={{
              flexGrow: 1,
              display: 'flex'
            }}
          >
            <DlEditor readOnly value={preview.datalogText} />
          </div>
        </div>
      )}
    </>
  );
}

DatalogList.propTypes = {
  datalog: PropTypes.array,
  onSelect: PropTypes.func
};

export default connect(
  ['datalogs', 'connections'],
  actions
)(React.memo(DatalogList));
