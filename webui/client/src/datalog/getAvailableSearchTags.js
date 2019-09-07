import uniq from 'lodash/uniq';

export default function getAvailableSearchTags(datalogs, connections) {
  const createdBys = uniq(datalogs.map(q => `createdBy=${q.createdBy}`));
  const tags = uniq(
    datalogs
      .map(q => q.tags)
      .reduce((a, b) => a.concat(b), [])
      .filter(tag => Boolean(tag))
      .map(tag => `tag=${tag}`)
  );

  return createdBys
    .concat(tags)
    .concat(connections.map(c => `connection=${c.name}`))
    .sort();
}
