import React from 'react';
import PropTypes from 'prop-types';
// import { VariableSizeGrid } from 'react-window';
import throttle from 'lodash/throttle';
import Draggable from 'react-draggable';
// import Measure from 'react-measure';
// import SpinKitCube from './SpinKitCube.js';
import moment from 'moment';
import SqlEditor from '../common/SqlEditor';
// import FlyingBirds from './FlyingBirds.js';
// import FoldingCube from './FoldingCube.js';
import RollingCircle from './RollingCircle.js';
import Button from 'antd/lib/button';

const renderValue = (input, fieldMeta) => {
  if (input === null || input === undefined) {
    return <em>null</em>;
  } else if (input === true || input === false) {
    return input.toString();
  } else if (fieldMeta.datatype === 'date') {
    return moment.utc(input).format('MM/DD/YYYY HH:mm:ss');
  } else if (typeof input === 'object') {
    return JSON.stringify(input, null, 2);
  } else {
    return input;
  }
};

// Hide the overflow so the scroll bar never shows in the header grid
// const headerStyle = {
//   overflowX: 'hidden',
//   overflowY: 'hidden'
// };

// NOTE: PureComponent's shallow compare works for this component
// because the isRunning prop will toggle with each datalog execution
// It would otherwise not rerender on change of prop.datalogResult alone
class DatalogResultDataTable extends React.PureComponent {
  state = {
    dimensions: {
      width: -1,
      height: -1
    },
    columnWidths: {}
  };

  static getDerivedStateFromProps(nextProps, prevState) {
    const { datalogResult } = nextProps;
    const { columnWidths } = prevState;

    if (datalogResult && datalogResult.fields) {
      datalogResult.fields.forEach(field => {
        if (!columnWidths[field]) {
          const fieldMeta = 'SQL'; //datalogResult.meta[field];
          let valueLength = fieldMeta.maxValueLength;

          if (field.length > valueLength) {
            valueLength = field.length;
          }
          let columnWidthGuess = valueLength * 20;
          if (columnWidthGuess < 100) {
            columnWidthGuess = 100;
          } else if (columnWidthGuess > 350) {
            columnWidthGuess = 350;
          }

          columnWidths[field] = columnWidthGuess;
        }
      });
    }
    return { columnWidths };
  }

  // NOTE
  // An empty dummy column is added to the grid for visual purposes
  // If dataKey was found this is a real column of data from the datalog result
  // If not, it's the dummy column at the end, and it should fill the rest of the grid width
  getColumnWidth = index => {
    const { columnWidths } = this.state;
    const { datalogResult } = this.props;
    const dataKey = datalogResult.fields[index];
    const { width } = this.state.dimensions;

    if (dataKey) {
      return columnWidths[dataKey];
    }

    const totalWidthFilled = datalogResult.fields
      .map(key => columnWidths[key])
      .reduce((prev, curr) => prev + curr, 0);

    const fakeColumnWidth = width - totalWidthFilled;
    return fakeColumnWidth < 10 ? 10 : fakeColumnWidth;
  };

  headerGrid = React.createRef();
  bodyGrid = React.createRef();

  resizeColumn = ({ dataKey, deltaX, columnIndex }) => {
    this.setState(
      prevState => {
        const prevWidths = prevState.columnWidths;
        const newWidth = prevWidths[dataKey] + deltaX;
        return {
          columnWidths: {
            ...prevWidths,
            [dataKey]: newWidth > 100 ? newWidth : 100
          }
        };
      },
      () => this.recalc(columnIndex)
    );
  };

  recalc = throttle(columnIndex => {
    if (this.headerGrid.current.resetAfterColumnIndex) {
      this.headerGrid.current.resetAfterColumnIndex(columnIndex);
      this.bodyGrid.current.resetAfterColumnIndex(columnIndex);
    }
  }, 100);

  HeaderCell = ({ columnIndex, rowIndex, style }) => {
    const { datalogResult } = this.props;
    const dataKey = datalogResult.fields[columnIndex];

    // If dataKey is present this is an actual header to render
    if (dataKey) {
      return (
        <div
          className={
            'flex bb b--moon-gray justify-between ph2 fw7 bg-near-white'
          }
          style={Object.assign({}, style, { lineHeight: '30px' })}
        >
          <div>{dataKey}</div>
          <Draggable
            axis="x"
            defaultClassName="DragHandle"
            defaultClassNameDragging="DragHandleActive"
            onDrag={(event, { deltaX }) => {
              this.resizeColumn({ dataKey, deltaX, columnIndex });
            }}
            position={{ x: 0 }}
            zIndex={999}
          >
            <span className="DragHandleIcon">⋮</span>
          </Draggable>
        </div>
      );
    }

    // If this is a dummy header cell render an empty header cell
    return (
      <div
        className={'flex bb b--moon-gray justify-between ph2 fw7 bg-near-white'}
        style={Object.assign({}, style, { lineHeight: '30px' })}
      />
    );
  };

  Cell = ({ columnIndex, rowIndex, style }) => {
    const { datalogResult } = this.props;
    const dataKey = datalogResult.fields[columnIndex];
    const backgroundColor = rowIndex % 2 === 0 ? 'bg-near-white' : '';

    // If dataKey is present this is a real data cell to render
    if (dataKey) {
      const fieldMeta = 'SQL'; // datalogResult.meta[dataKey];

      // Account for extra row that was used for header row
      const value = datalogResult.sql; // datalogResult.rows[rowIndex][dataKey];

      return (
        <div
          className={'relative bb b--light-gray ph2 ' + backgroundColor}
          style={Object.assign({}, style, { lineHeight: '30px' })}
        >
          <div className="truncate">{renderValue(value, fieldMeta)}</div>
        </div>
      );
    }

    // If no dataKey this is a dummy cell.
    // It should render nothing, but match the row's style
    return (
      <div
        className={'relative bb b--light-gray ph2 ' + backgroundColor}
        style={Object.assign({}, style, { lineHeight: '30px' })}
      >
        <div className="truncate" />
      </div>
    );
  };

  getRowHeight() {
    return 30;
  }

  // When a scroll occurs in the body grid,
  // synchronize the scroll position of the header grid
  handleGridScroll = ({ scrollLeft }) => {
    this.headerGrid.current.scrollTo({ scrollLeft });
  };

  handleContainerResize = contentRect => {
    this.setState({ dimensions: contentRect.bounds });
  };

  render() {
    const { isRunning, datalogError, datalogResult, runDatalog } = this.props;
    // const { height, width } = this.state.dimensions;

    if (isRunning) {
      return (
        // <div className="aspect-ratio--object flex items-center justify-center">
        //   <FlyingBirds />
        // </div>
        <div className="h-100 items-center flex-center">
          {/* <FoldingCube /> */}
          <RollingCircle />
        </div>
      );
    }

    if (datalogError) {
      if (datalogError.includes ('not validated')){
        return (
          <div
            className={`aspect-ratio--object items-center justify-center f3 pa4 tc bg-light-yellow`}
          >
            {datalogError}
            <br/>
            <Button
              type="primary"
              onClick={() => runDatalog(5, false)}
              disabled={isRunning}
            >
              Generate a counterexample
            </Button>
          </div>
        );
      }
      else {
        return (
          <div
            className={`aspect-ratio--object items-center justify-center f3 pa4 tc bg-light-yellow`}
          >
            {datalogError}
          </div>
        );
      }
    }

    if (datalogResult) {
      //&& datalogResult.rows
      // const rowCount = 1; // datalogResult.rows.length;
      // Add extra column to fill remaining grid width if necessary
      // const columnCount = datalogResult.fields.length + 1;

      // return (
      //   <Measure bounds onResize={this.handleContainerResize}>
      //     {({ measureRef }) => (
      //       <div ref={measureRef} className="h-100 w-100 aspect-ratio--object ">
      //         <VariableSizeGrid
      //           columnCount={columnCount}
      //           rowCount={1}
      //           columnWidth={this.getColumnWidth}
      //           rowHeight={this.getRowHeight}
      //           height={30}
      //           width={width}
      //           ref={this.headerGrid}
      //           style={headerStyle}
      //         >
      //           {this.HeaderCell}
      //         </VariableSizeGrid>
      //         <VariableSizeGrid
      //           columnCount={columnCount}
      //           rowCount={rowCount}
      //           columnWidth={this.getColumnWidth}
      //           rowHeight={this.getRowHeight}
      //           width={width}
      //           height={height - 30}
      //           ref={this.bodyGrid}
      //           onScroll={this.handleGridScroll}
      //         >
      //           {this.Cell}
      //         </VariableSizeGrid>
      //       </div>
      //     )}
      //   </Measure>
      // );

      return (
        <SqlEditor
          value={datalogResult.sql}
          readOnly={true}
          // onChange={onChange}
          // onSelectionChange={handleDatalogSelectionChange}
        />
      );
    }

    return <div className="aspect-ratio--object" />;
  }
}

DatalogResultDataTable.propTypes = {
  isRunning: PropTypes.bool,
  datalogError: PropTypes.string,
  datalogResult: PropTypes.object,
  runDatalog: PropTypes.func.isRequired
};

export default DatalogResultDataTable;
