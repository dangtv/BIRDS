import createStore from 'unistore';
import uuid from 'uuid';
import sortBy from 'lodash/sortBy';
import message from 'antd/lib/message';
import sqlFormatter from 'sql-formatter';
import dlFormatter from 'sql-formatter';
import fetchJson from '../utilities/fetch-json.js';
import updateCompletions from '../utilities/updateCompletions.js';

const ONE_HOUR_MS = 1000 * 60 * 60;

function sortConnections(connections) {
  return sortBy(connections, [connection => connection.name.toLowerCase()]);
}

const NEW_QUERY = {
  _id: '',
  name: '',
  tags: [],
  connectionId: '',
  queryText: '',
  chartConfiguration: {
    chartType: '',
    fields: {} // key value for chart
  }
};

const NEW_DATALOG = {
  _id: '',
  name: '',
  tags: [],
  connectionId: '',
  datalogText: '',
  chartConfiguration: {
    chartType: '',
    fields: {} // key value for chart
  }
};

export const unistoreStore = createStore({
  selectedConnectionId: 'noconnection',
  connections: [],
  connectionsLastUpdated: null,
  connectionsLoading: false,
  availableTags: [],
  cacheKey: uuid.v1(),
  isRunning: false,
  isSaving: false,
  queries: [],
  query: Object.assign({}, NEW_QUERY),
  queryResult: undefined,
  queryError: null,
  runQueryStartTime: undefined,
  datalogs: [],
  datalog: Object.assign({}, NEW_DATALOG),
  datalogResult: undefined,
  datalogError: null,
  runDatalogStartTime: undefined,
  selectedText: '',
  timeout: '60',
  verification: false,
  optimization: false,
  speedup: false,
  showValidation: false,
  showSchema: true,
  showVisSidebar: false,
  unsavedChanges: false,
  schema: {} // schema.<connectionId>.loading / schemaInfo / lastUpdated
});

// If actions is a function, it gets passed the store:
// Actions receive current state as first parameter and any other params next
// Actions can just return a state update:
export const actions = store => ({
  // APP NAV
  toggleSchema(state) {
    return {
      showSchema: !state.showSchema,
      showVisSidebar: false
    };
  },

  toggleVisSidebar(state) {
    return {
      showVisSidebar: !state.showVisSidebar,
      showSchema: false
    };
  },

  // CONFIG
  async refreshAppContext() {
    const json = await fetchJson('GET', 'api/app');
    if (!json.config) {
      return;
    }
    // Assign config.baseUrl to global
    // It doesn't change and is needed for fetch requests
    // This allows us to simplify the fetch() call
    window.BASE_URL = json.config.baseUrl;

    return {
      config: json.config,
      smtpConfigured: json.smtpConfigured,
      googleAuthConfigured: json.googleAuthConfigured,
      currentUser: json.currentUser,
      passport: json.passport,
      adminRegistrationOpen: json.adminRegistrationOpen,
      version: json.version
    };
  },

  // SCHEMA
  async loadSchemaInfo(state, connectionId, reload) {
    const { schema } = state;
    if (!schema[connectionId] || reload) {
      store.setState({
        schema: {
          ...schema,
          [connectionId]: {
            loading: true,
            expanded: {}
          }
        }
      });

      
      if (connectionId == 'noconnection'){
        return {
          schema: {
            ...schema,
            [connectionId]: {
              loading: false,
              // schemaInfo,
              // expanded
            }
          }
        };
      }
      else{
        const qs = reload ? '?reload=true' : '';
        const json = await fetchJson(
          'GET',
          `/api/schema-info/${connectionId}${qs}`
        )
      
        const { error, schemaInfo } = json;
        if (error) {
          return message.error(error);
        }
        updateCompletions(schemaInfo);

        // Pre-expand schemas
        const expanded = {};
        if (schemaInfo) {
          Object.keys(schemaInfo).forEach(schemaName => {
            expanded[schemaName] = true;
          });
        }

        return {
          schema: {
            ...schema,
            [connectionId]: {
              loading: false,
              schemaInfo,
              expanded
            }
          }
        };
      }
    }
  },

  toggleSchemaItem(state, connectionId, item) {
    const { schema } = state;
    const connectionSchema = schema[connectionId];
    const open = !connectionSchema.expanded[item.id];
    return {
      schema: {
        ...schema,
        [connectionId]: {
          ...connectionSchema,
          expanded: { ...connectionSchema.expanded, [item.id]: open }
        }
      }
    };
  },

  // Timeout option
  selectTimeout(state, timeout) {
    return { timeout };
  },

  // Verification option
  setVerification(state, verification) {
    return { verification };
  },

  // Optimization option
  setOptimization(state, optimization) {
    return { optimization };
  },

  // Speed-up option
  setSpeedup(state, speedup) {
    return { speedup };
  },

  // CONNECTIONS
  selectConnectionId(state, selectedConnectionId) {
    return { selectedConnectionId, unsavedChanges: true };
  },

  async deleteConnection(state, connectionId) {
    const { connections } = state;
    const json = await fetchJson('DELETE', '/api/connections/' + connectionId);
    if (json.error) {
      return message.error('Delete failed');
    }
    const filtered = connections.filter(c => c._id !== connectionId);
    return { connections: sortConnections(filtered) };
  },

  // Updates store (is not resonponsible for API call)
  async addUpdateConnection(state, connection) {
    const { connections } = state;
    const found = connections.find(c => c._id === connection._id);
    if (found) {
      const mappedConnections = connections.map(c => {
        if (c._id === connection._id) {
          return connection;
        }
        return c;
      });
      return { connections: sortConnections(mappedConnections) };
    }
    return { connections: sortConnections([connection].concat(connections)) };
  },

  async loadConnections(state, force) {
    const { connections, connectionsLoading, connectionsLastUpdated } = state;
    if (connectionsLoading) {
      return;
    }

    if (
      force ||
      !connections.length ||
      (connectionsLastUpdated &&
        new Date() - connectionsLastUpdated > ONE_HOUR_MS)
    ) {
      store.setState({ connectionsLoading: true });
      const { error, connections } = await fetchJson(
        'GET',
        '/api/connections/'
      );
      if (error) {
        message.error(error);
      }
      const update = {
        connectionsLoading: false,
        connectionsLastUpdated: new Date(),
        connections: sortConnections(connections)
      };

      if (connections && connections.length === 1) {
        update.selectedConnectionId = connections[0]._id;
      }

      store.setState(update);
    }
  },

  // QUERY
  formatQuery(state) {
    const { query } = state;
    return {
      query: { ...query, queryText: sqlFormatter.format(query.queryText) },
      unsavedChanges: true
    };
  },

  async loadQueries(state) {
    const { queriesLastUpdated, queries } = state;
    if (
      !queries.length ||
      (queriesLastUpdated && new Date() - queriesLastUpdated > ONE_HOUR_MS)
    ) {
      store.setState({ queriesLoading: true });
      const json = await fetchJson('GET', '/api/queries');
      if (json.error) {
        message.error(json.error);
      }
      store.setState({
        queriesLoading: false,
        queriesLastUpdated: new Date(),
        queries: json.queries || []
      });
    }
  },

  async deleteQuery(state, queryId) {
    const { queries } = state;
    const filteredQueries = queries.filter(q => {
      return q._id !== queryId;
    });
    store.setState({ queries: filteredQueries });
    const json = await fetchJson('DELETE', '/api/queries/' + queryId);
    if (json.error) {
      message.error(json.error);
      store.setState({ queries });
    }
  },

  async loadQuery(state, queryId) {
    const { error, query } = await fetchJson('GET', `/api/queries/${queryId}`);
    if (error) {
      message.error(error);
    }
    return { query, selectedConnectionId: query.connectionId };
  },

  async loadTags(state) {
    const { error, tags } = await fetchJson('GET', '/api/tags');
    if (error) {
      message.error(error);
    }
    return { availableTags: tags };
  },

  async runQuery(state) {
    const { cacheKey, query, selectedText, selectedConnectionId } = state;

    store.setState({
      isRunning: true,
      runQueryStartTime: new Date()
    });
    const postData = {
      connectionId: selectedConnectionId,
      cacheKey,
      queryName: query.name,
      queryText: selectedText || query.queryText
    };
    const { queryResult, error } = await fetchJson(
      'POST',
      '/api/query-result',
      postData
    );
    if (error) {
      message.error(error);
    }
    store.setState({
      isRunning: false,
      queryError: error,
      queryResult
    });
  },

  saveQuery(state) {
    const { query, selectedConnectionId } = state;
    if (!query.name) {
      message.error('Query name required');
      store.setState({ showValidation: true });
      return;
    }
    store.setState({ isSaving: true });
    const queryData = Object.assign({}, query, {
      connectionId: selectedConnectionId
    });
    if (query._id) {
      fetchJson('PUT', `/api/queries/${query._id}`, queryData).then(json => {
        const { error, query } = json;
        const { queries } = store.getState();
        if (error) {
          message.error(error);
          store.setState({ isSaving: false });
          return;
        }
        message.success('Query Saved');
        const updatedQueries = queries.map(q => {
          return q._id === query._id ? query : q;
        });
        store.setState({
          isSaving: false,
          unsavedChanges: false,
          query,
          queries: updatedQueries
        });
      });
    } else {
      fetchJson('POST', `/api/queries`, queryData).then(json => {
        const { error, query } = json;
        const { queries } = store.getState();
        if (error) {
          message.error(error);
          store.setState({ isSaving: false });
          return;
        }
        window.history.replaceState(
          {},
          query.name,
          `${window.BASE_URL}/queries/${query._id}`
        );
        message.success('Query Saved');
        store.setState({
          isSaving: false,
          unsavedChanges: false,
          query,
          queries: [query].concat(queries)
        });
      });
    }
  },

  handleCloneClick(state) {
    const { query } = state;
    delete query._id;
    const name = 'Copy of ' + query.name;
    window.history.replaceState({}, name, `${window.BASE_URL}/queries/new`);
    return { query: { ...query, name }, unsavedChanges: true };
  },

  handleCloneClickDatalog(state) {
    const { datalog } = state;
    delete datalog._id;
    const name = 'Copy of ' + datalog.name;
    window.history.replaceState({}, name, `${window.BASE_URL}/datalog/new`);
    return { datalog: { ...datalog, name }, unsavedChanges: true };
  },

  resetNewQuery(state) {
    return {
      queryResult: undefined,
      query: Object.assign({}, NEW_QUERY),
      unsavedChanges: false
    };
  },

  setQueryState(state, field, value) {
    const { query } = state;
    return { query: { ...query, [field]: value }, unsavedChanges: true };
  },

  handleChartConfigurationFieldsChange(state, chartFieldId, queryResultField) {
    const { query } = state;
    const { fields } = query.chartConfiguration;
    return {
      query: {
        ...query,
        chartConfiguration: {
          ...query.chartConfiguration,
          fields: { ...fields, [chartFieldId]: queryResultField }
        }
      },
      unsavedChanges: true
    };
  },

  handleChartTypeChange(state, chartType) {
    const { query } = state;
    return {
      query: {
        ...query,
        chartConfiguration: { ...query.chartConfiguration, chartType }
      },
      unsavedChanges: true
    };
  },

  handleQuerySelectionChange(state, selectedText) {
    return { selectedText };
  },

  // DATALOG 
  formatDatalog(state) {
    const { datalog } = state;
    return {
      datalog: { ...datalog, datalogText: dlFormatter.format(datalog.datalogText) },
      unsavedChanges: true
    };
  },

  async loadDatalogs(state) {
    const { datalogsLastUpdated, datalogs } = state;
    if (
      !datalogs.length ||
      (datalogsLastUpdated && new Date() - datalogsLastUpdated > ONE_HOUR_MS)
    ) {
      store.setState({ datalogsLoading: true });
      const json = await fetchJson('GET', '/api/datalog');
      if (json.error) {
        message.error(json.error);
      }
      store.setState({
        datalogsLoading: false,
        datalogsLastUpdated: new Date(),
        datalogs: json.datalogs || []
      });
    }
  },

  async deleteDatalog(state, datalogId) {
    const { datalogs } = state;
    const filteredDatalogs = datalogs.filter(q => {
      return q._id !== datalogId;
    });
    store.setState({ datalogs: filteredDatalogs });
    const json = await fetchJson('DELETE', '/api/datalog/' + datalogId);
    if (json.error) {
      message.error(json.error);
      store.setState({ datalogs });
    }
  },

  async loadDatalog(state, datalogId) {
    const { error, datalog } = await fetchJson('GET', `/api/datalog/${datalogId}`);
    if (error) {
      message.error(error);
    }
    return { datalog, selectedConnectionId: datalog.connectionId};
  },

  async runDatalog(state) {
    const { cacheKey, datalog, selectedText, selectedConnectionId, timeout, verification, optimization, speedup } = state;

    store.setState({
      isRunning: true,
      runDatalogStartTime: new Date()
    });
    const postData = {
      connectionId: selectedConnectionId,
      timeout: timeout,
      verification: verification,
      optimization: optimization,
      speedup: speedup,
      cacheKey,
      datalogName: datalog.name,
      datalogText: selectedText || datalog.datalogText
    };
    const { datalogResult, error } = await fetchJson(
      'POST',
      '/api/datalog-result',
      postData
    );
    if (error) {
      message.error(error);
    }
    store.setState({
      isRunning: false,
      datalogError: error,
      datalogResult
    });
  },

  saveDatalog(state) {
    const { datalog, selectedConnectionId } = state;
    if (!datalog.name) {
      message.error('Datalog name required');
      store.setState({ showValidation: true });
      return;
    }
    store.setState({ isSaving: true });
    const datalogData = Object.assign({}, datalog, {
      connectionId: selectedConnectionId
    });
    if (datalog._id) {
      fetchJson('PUT', `/api/datalog/${datalog._id}`, datalogData).then(json => {
        const { error, datalog } = json;
        const { datalogs } = store.getState();
        if (error) {
          message.error(error);
          store.setState({ isSaving: false });
          return;
        }
        message.success('Datalog Saved');
        const updatedDatalogs = datalogs.map(q => {
          return q._id === datalog._id ? datalog : q;
        });
        store.setState({
          isSaving: false,
          unsavedChanges: false,
          datalog,
          datalogs: updatedDatalogs
        });
      });
    } else {
      fetchJson('POST', `/api/datalog`, datalogData).then(json => {
        const { error, datalog } = json;
        const { datalogs } = store.getState();
        if (error) {
          message.error(error);
          store.setState({ isSaving: false });
          return;
        }
        window.history.replaceState(
          {},
          datalog.name,
          `${window.BASE_URL}/datalog/${datalog._id}`
        );
        message.success('Datalog Saved');
        store.setState({
          isSaving: false,
          unsavedChanges: false,
          datalog,
          datalogs: [datalog].concat(datalogs)
        });
      });
    }
  },

  resetNewDatalog(state) {
    return {
      datalogResult: undefined,
      datalog: Object.assign({}, NEW_DATALOG),
      unsavedChanges: false
    };
  },

  async importSchemaDatalog(state) {
    const { selectedConnectionId } = state;
      const {datalogSchema } = await fetchJson(
        'GET',
        `/api/schema-datalog/${selectedConnectionId}`
      );
      // if (error) {
      //   return message.error(error);
      // }

      return {
      datalogResult: undefined,
      datalog: Object.assign({}, {
        _id: '',
        name: '',
        tags: [],
        connectionId: '',
        datalogText: datalogSchema,
        chartConfiguration: {
          chartType: '',
          fields: {} // key value for chart
        }
      }),
      unsavedChanges: false
      };
  },

  setDatalogState(state, field, value) {
    const { datalog } = state;
    return { datalog: { ...datalog, [field]: value }, unsavedChanges: true };
  },

  handleDatalogSelectionChange(state, selectedText) {
    return { selectedText };
  }
});
