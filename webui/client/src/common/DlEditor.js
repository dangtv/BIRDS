// NOTE this import 'brace' must occur before the importing of brace extensions
import 'brace';
import 'brace/ext/searchbox';
import 'brace/mode/prolog';
import 'brace/theme/xcode';
import PropTypes from 'prop-types';
import React, { useState, useEffect } from 'react';
import { connect } from 'unistore/react';
import { actions } from '../stores/unistoreStore';
import Measure from 'react-measure';
import AceEditor from 'react-ace';

const noop = () => {};

function DlEditor({ config, onChange, readOnly, value, onSelectionChange }) {
  const [dimensions, setDimensions] = useState({ width: -1, height: -1 });
  const [editor, setEditor] = useState(null);

  useEffect(() => {
    if (editor && onChange) {
      // augment the built-in behavior of liveAutocomplete
      // built-in behavior only starts autocomplete when at least 1 character has been typed
      // In ace the . resets the prefix token and clears the completer
      // In order to get completions for 'sometable.' we need to fire the completer manually
      editor.commands.on('afterExec', e => {
        if (e.command.name === 'insertstring' && /^[\w.]$/.test(e.args)) {
          if (e.args === '.') {
            editor.execCommand('startAutocomplete');
          }
        }
      });

      editor.session.setUseWrapMode(Boolean(config.editorWordWrap));
      editor.setOptions({
        fontSize: Number(config.fontSize),
        showLineNumbers: Boolean(config.showLineNumbers),
        showGutter: Boolean(config.showLineNumbers)
      });
      // editor.setFontSize(Number(config.fontSize));
      // editor.renderer.setOption('showLineNumbers',Boolean(config.showLineNumbers));
    }
  }, [editor, onChange, config]);

  const handleSelection = selection => {
    if (editor && editor.session) {
      const selectedText = editor.session.getTextRange(selection.getRange());
      onSelectionChange(selectedText);
    }
  };

  const { width, height } = dimensions;

  return (
    <Measure bounds onResize={contentRect => setDimensions(contentRect.bounds)}>
      {({ measureRef }) => (
        <div ref={measureRef} className="h-100 w-100">
          <AceEditor
            focus={true}
            editorProps={{ $blockScrolling: Infinity }}
            enableBasicAutocompletion
            enableLiveAutocompletion
            height={height + 'px'}
            highlightActiveLine={false}
            mode="prolog"
            name="query-ace-editor"
            onLoad={editor => setEditor(editor)}
            onChange={onChange || noop}
            onSelectionChange={handleSelection}
            showGutter={false}
            showPrintMargin={false}
            theme="xcode"
            readOnly={readOnly}
            value={value}
            width={width + 'px'}
          />
        </div>
      )}
    </Measure>
  );
}

DlEditor.propTypes = {
  onChange: PropTypes.func,
  onSelectionChange: PropTypes.func,
  readOnly: PropTypes.bool,
  value: PropTypes.string
};

DlEditor.defaultProps = {
  onSelectionChange: () => {},
  readOnly: false,
  value: ''
};

export default connect(
  ['config'],
  actions
)(React.memo(DlEditor));
