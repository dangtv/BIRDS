const assert = require('assert');
const getMeta = require('../../lib/getMeta.js');

const d1 = new Date();
const d2 = new Date(new Date().getTime() + 60000);

describe('lib/getMeta.js', function() {
  it('returns expected results', function() {
    const rows = [
      // To ensure nulls are handled appropriately start and end with them
      {
        alwaysNull: null,
        accountNumber: null,
        decimalString: null,
        number: null,
        string: null,
        date: null,
        numberString: null
      },
      {
        alwaysNull: null,
        accountNumber: '099999',
        decimalString: '0.999',
        number: 30,
        string: 'abcdefg',
        date: d2,
        numberString: 100
      },
      {
        alwaysNull: null,
        accountNumber: '0111',
        decimalString: '0.111',
        number: 0,
        string: '0',
        date: d1,
        numberString: 0
      },
      {
        alwaysNull: null,
        accountNumber: null,
        decimalString: null,
        number: null,
        string: 'abc',
        date: null,
        numberString: null
      }
    ];

    const meta = getMeta(rows);

    assert.equal(meta.alwaysNull.datatype, null, 'null');

    assert.equal(meta.accountNumber.datatype, 'string', 'accountNumber');
    assert.equal(
      meta.accountNumber.maxValueLength,
      6,
      'accountNumber.maxValueLength'
    );

    assert.equal(meta.decimalString.datatype, 'number', 'decimalString');
    assert.equal(meta.decimalString.max, 0.999, 'decimalString.max');
    assert.equal(meta.decimalString.min, 0.111, 'decimalString.min');

    assert.equal(meta.number.datatype, 'number', 'number.datatype');
    assert.equal(meta.number.max, 30, 'number.max');
    assert.equal(meta.number.min, 0, 'number.min');

    assert.equal(meta.string.datatype, 'string', 'string.datatype');
    assert.equal(meta.string.maxValueLength, 7, 'string.maxValueLength');

    assert.equal(meta.date.datatype, 'date', 'date.datatype');
    assert.equal(meta.date.max.getTime(), d2.getTime(), 'date.max');
    assert.equal(meta.date.min.getTime(), d1.getTime(), 'date.min');

    assert.equal(meta.numberString.datatype, 'number', 'numberString.datatype');
    assert.equal(meta.numberString.max, 100, 'numberString.max');
    assert.equal(meta.numberString.min, 0, 'numberString.min');
  });
});
