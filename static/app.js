// PLC Simulator JavaScript Application

let updateInterval = null;
let isRunning = false;

// Initialize UI on page load
document.addEventListener('DOMContentLoaded', function() {
    initializeIO();
    updateState();
    // Auto-refresh state every 200ms when running
    setInterval(function() {
        if (isRunning) {
            updateState();
            refreshLadder();
        }
    }, 200);
});

// Initialize I/O grids
function initializeIO() {
    const inputsGrid = document.getElementById('inputs-grid');
    const outputsGrid = document.getElementById('outputs-grid');
    
    // Create 16 inputs (X0-X15)
    for (let i = 0; i < 16; i++) {
        const item = document.createElement('div');
        item.className = 'io-item';
        
        const button = document.createElement('button');
        button.className = 'io-button';
        button.id = `input-${i}`;
        button.textContent = '0';
        button.onclick = () => toggleInput(i);
        
        const label = document.createElement('div');
        label.className = 'io-label';
        label.textContent = `X${i}`;
        
        item.appendChild(button);
        item.appendChild(label);
        inputsGrid.appendChild(item);
    }
    
    // Create 16 outputs (Y0-Y15)
    for (let i = 0; i < 16; i++) {
        const item = document.createElement('div');
        item.className = 'io-item';
        
        const indicator = document.createElement('div');
        indicator.className = 'io-indicator';
        indicator.id = `output-${i}`;
        indicator.textContent = '0';
        
        const label = document.createElement('div');
        label.className = 'io-label';
        label.textContent = `Y${i}`;
        
        item.appendChild(indicator);
        item.appendChild(label);
        outputsGrid.appendChild(item);
    }
}

// API Calls

async function apiCall(endpoint, data = null) {
    try {
        const options = {
            method: data ? 'POST' : 'GET',
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded'
            }
        };
        
        if (data) {
            const params = new URLSearchParams(data);
            options.body = params;
        }
        
        const response = await fetch(endpoint, options);
        return await response.json();
    } catch (error) {
        console.error('API call failed:', error);
        showError(`API Error: ${error.message}`);
        return null;
    }
}

// Program Management

async function loadProgramFromEditor() {
    const editor = document.getElementById('program-editor');
    const program = editor.value;
    
    if (!program.trim()) {
        showError('Please enter a program first');
        return;
    }
    
    const result = await apiCall('/api/load-program', { program: program });
    
    if (result && result.success) {
        clearError();
        showMessage('Program loaded successfully!');
        refreshLadder();
    } else if (result && result.error) {
        showError(result.error);
    }
}

function clearEditor() {
    document.getElementById('program-editor').value = '';
    clearError();
}

// Execution Control

async function runProgram() {
    const result = await apiCall('/api/run');
    if (result && result.success) {
        isRunning = true;
        updateExecutionStatus('running');
    }
}

async function stopProgram() {
    const result = await apiCall('/api/stop');
    if (result && result.success) {
        isRunning = false;
        updateExecutionStatus('stopped');
        updateState();
    }
}

async function stepProgram() {
    const result = await apiCall('/api/step');
    if (result && result.success) {
        updateState();
        refreshLadder();
    }
}

async function resetPLC() {
    const result = await apiCall('/api/reset');
    if (result && result.success) {
        isRunning = false;
        updateExecutionStatus('stopped');
        updateState();
        showMessage('PLC reset');
    }
}

// I/O Control

async function toggleInput(id) {
    const result = await apiCall('/api/toggle-input', { id: id });
    if (result && result.success) {
        updateState();
    }
}

// State Updates

async function updateState() {
    const result = await apiCall('/api/state');
    
    if (result && result.success) {
        // Update inputs
        if (result.inputs) {
            result.inputs.forEach((value, index) => {
                const button = document.getElementById(`input-${index}`);
                if (button) {
                    button.textContent = value;
                    if (value === 1) {
                        button.classList.add('on');
                    } else {
                        button.classList.remove('on');
                    }
                }
            });
        }
        
        // Update outputs
        if (result.outputs) {
            result.outputs.forEach((value, index) => {
                const indicator = document.getElementById(`output-${index}`);
                if (indicator) {
                    indicator.textContent = value;
                    if (value === 1) {
                        indicator.classList.add('on');
                    } else {
                        indicator.classList.remove('on');
                    }
                }
            });
        }
        
        // Update execution status
        if (result.executionState) {
            updateExecutionStatus(result.executionState);
        }
        
        // Update scan count
        if (result.scanCount !== undefined) {
            document.getElementById('scan-count').textContent = result.scanCount;
        }
    }
}

function updateExecutionStatus(state) {
    const statusElement = document.getElementById('execution-status');
    statusElement.className = '';
    
    switch(state) {
        case 'running':
            statusElement.textContent = 'RUNNING';
            statusElement.classList.add('status-running');
            break;
        case 'single-step':
            statusElement.textContent = 'STEP';
            statusElement.classList.add('status-single-step');
            break;
        default:
            statusElement.textContent = 'STOPPED';
            statusElement.classList.add('status-stopped');
            isRunning = false;
    }
}

// Ladder Diagram

async function refreshLadder() {
    const ladderDisplay = document.getElementById('ladder-display');
    
    try {
        const response = await fetch('/api/ladder');
        const svg = await response.text();
        
        if (svg.includes('<svg')) {
            ladderDisplay.innerHTML = svg;
        } else {
            ladderDisplay.innerHTML = '<div class="no-program">No program loaded</div>';
        }
    } catch (error) {
        console.error('Failed to refresh ladder:', error);
        ladderDisplay.innerHTML = '<div class="no-program">Error loading ladder diagram</div>';
    }
}

// Error/Message Display

function showError(message) {
    const errorElement = document.getElementById('error-message');
    errorElement.textContent = message;
    errorElement.classList.add('show');
}

function clearError() {
    const errorElement = document.getElementById('error-message');
    errorElement.textContent = '';
    errorElement.classList.remove('show');
}

function showMessage(message) {
    // Simple alert for now, could be enhanced with a toast notification
    clearError();
    console.log('Message:', message);
}

// File Upload/Download

async function uploadFile(event) {
    const file = event.target.files[0];
    if (!file) return;
    
    const reader = new FileReader();
    reader.onload = async function(e) {
        const content = e.target.result;
        const result = await apiCall('/api/load-program', { program: content });
        
        if (result && result.success) {
            document.getElementById('program-editor').value = content;
            clearError();
            showMessage('File loaded successfully!');
            refreshLadder();
        } else if (result && result.error) {
            showError(result.error);
        }
    };
    reader.readAsText(file);
    
    // Reset file input
    event.target.value = '';
}

function downloadProgram() {
    const program = document.getElementById('program-editor').value;
    if (!program.trim()) {
        showError('No program to download');
        return;
    }
    
    const blob = new Blob([program], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'program.il';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    
    showMessage('Program downloaded');
}

// Examples Management

async function showExamples() {
    const result = await apiCall('/api/examples');
    
    if (result && result.success && result.examples) {
        const examplesPanel = document.getElementById('examples-panel');
        const examplesList = document.getElementById('examples-list');
        
        examplesList.innerHTML = '';
        
        result.examples.forEach(example => {
            const item = document.createElement('div');
            item.className = 'example-item';
            item.textContent = example.name.replace('.il', '');
            item.onclick = () => loadExample(example.name);
            examplesList.appendChild(item);
        });
        
        examplesPanel.style.display = 'block';
    } else {
        showError('Failed to load examples list');
    }
}

function hideExamples() {
    document.getElementById('examples-panel').style.display = 'none';
}

async function loadExample(filename) {
    const result = await apiCall('/api/load-example', { filename: filename });
    
    if (result && result.success) {
        document.getElementById('program-editor').value = result.program;
        clearError();
        hideExamples();
        showMessage('Example loaded: ' + filename);
        refreshLadder();
    } else if (result && result.error) {
        showError(result.error);
    }
}
