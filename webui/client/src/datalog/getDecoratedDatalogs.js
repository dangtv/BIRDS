/**
 * Get array of datalogs decorated with `connectionName` property
 * @param {array} datalogs
 * @param {array} connections
 */
export default function getDecoratedDatalogs(datalogs, connections) {
  // Create index of lookups
  // TODO this should come from API
  const connectionsById = connections.reduce((connMap, connection) => {
    connMap[connection._id] = connection;
    return connMap;
  }, {});

  return datalogs.map(datalog => {
    datalog.key = datalog._id;

    const connection = connectionsById[datalog.connectionId];
    datalog.connectionName = connection ? connection.name : '';

    return datalog;
  });
}
