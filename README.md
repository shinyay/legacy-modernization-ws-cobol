# COBOL Development Container Sample

This is a sample project demonstrating COBOL development using Visual Studio Code Dev Containers. The project includes a simple COBOL program that displays a welcome message and the current date, as well as a comprehensive Syllabus Management System showcasing typical legacy COBOL application patterns.

## Prerequisites

- [Visual Studio Code](https://code.visualstudio.com/)
- [Docker](https://www.docker.com/)
- [Dev Containers Extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

## Getting Started

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/hello-devcontainer-with-cobol.git
   cd hello-devcontainer-with-cobol
   ```

2. Open the project in Visual Studio Code:
   ```bash
   code .
   ```

3. When prompted "Reopen in Container" by VS Code, click on it.
   - Alternatively, press `F1`, type "Dev Containers: Reopen in Container" and press Enter

4. Wait for the Dev Container to build and start. This may take a few minutes the first time.

## Building and Running the Application

Once inside the Dev Container, you can build and run the application using VS Code tasks:

### Using VS Code Command Palette (Ctrl+Shift+P or Cmd+Shift+P):
1. Build the application:
   - Open Command Palette and type "Tasks: Run Build Task" or press `Ctrl+Shift+B`

2. Run the application:
   - Open Command Palette
   - Type "Tasks: Run Task"
   - Select "run"

### Using Terminal in VS Code:
1. Build the application:
   ```bash
   make
   ```

2. Run the application:
   ```bash
   make run
   ```

3. Clean build artifacts:
   ```bash
   make clean
   ```

## Project Structure

- `src/hello.cob`: Simple demo COBOL program that displays welcome message and current date
- `src/SYLABUS.cbl`: Main program (controller) of the Syllabus Management System
- `src/SYLREG.cbl`: Syllabus Registration Program
- `src/SYLUPD.cbl`: Syllabus Update Program
- `src/SYLDEL.cbl`: Syllabus Delete Program
- `src/SYLQRY.cbl`: Syllabus Query Program
- `src/SYLLST.cbl`: Syllabus Listing Program
- `src/SYLRPT.cbl`: Syllabus Report Generation Program
- `src/SYLCOM.cbl`: Common Routines (Subprogram)
- `src/copybooks/`: Directory containing record definitions
  - `SYLFILE.cpy`: Syllabus file record definition
  - `DEPFILE.cpy`: Department file record definition
  - `TEAFILE.cpy`: Teacher file record definition
- `Makefile`: Build configuration
- `.devcontainer/`: Dev Container configuration files
- `bin/`: Build output directory (created during build)

# Syllabus Management System

The Syllabus Management System is a comprehensive application that demonstrates classic COBOL enterprise application patterns and techniques. This sample recreates many features found in traditional legacy systems that are commonly seen in educational institutions, financial services, and insurance companies.

## System Architecture

The system employs a hierarchical program structure with modular design — a traditional COBOL development approach where each program specializes in a specific function:

### Main Modules

1. **SYLABUS.cbl** - Main Controller Program
   - Serves as the entry point and main menu
   - Orchestrates calls to other specialized modules
   - Handles user input for menu navigation

2. **SYLREG.cbl** - Syllabus Registration Program
   - Implements the Create operation of CRUD
   - Provides screen interfaces for data entry
   - Validates input data through SYLCOM calls
   - Writes new syllabus records to the indexed file

3. **SYLUPD.cbl** - Syllabus Update Program
   - Implements the Update operation of CRUD
   - Allows modification of existing syllabus records
   - Provides field-by-field update options
   - Uses dynamic file access mode for record retrieval and update

4. **SYLDEL.cbl** - Syllabus Delete Program
   - Implements the Delete operation of CRUD
   - Provides confirmation before deletion
   - Removes syllabus records from indexed file
   - Handles deletion error cases

5. **SYLQRY.cbl** - Syllabus Query Program
   - Implements the Read operation of CRUD
   - Retrieves and displays detailed information about a specific syllabus
   - Shows both basic information and weekly plan details
   - Uses screen-oriented display with pagination

6. **SYLLST.cbl** - Syllabus Listing Program
   - Provides list functionality with filtering options
   - Implements pagination for viewing multiple records
   - Offers department, teacher, and semester-based filtering
   - Uses dynamic record counting and display

7. **SYLRPT.cbl** - Report Generation Program
   - Creates formatted reports from syllabus data
   - Offers multiple report types and filtering options
   - Outputs to sequential text files for printing
   - Includes header, detail, and summary sections

8. **SYLCOM.cbl** - Common Utilities Subprogram
   - Provides shared functionality for other modules
   - Implements data validation routines
   - Offers date handling and formatting services
   - Uses parameter passing for flexible operations

## Core COBOL Features Demonstrated

### 1. Screen Section for Text-Based UI

The application extensively uses the COBOL Screen Section to create text-based interfaces typical of traditional terminal applications:

```cobol
SCREEN SECTION.
01 SYLLABUS-INPUT-SCREEN.
    05 BLANK SCREEN.
    05 LINE 1 COLUMN 1 VALUE "シラバス登録画面".
    05 LINE 3 COLUMN 1 VALUE "科目コード (例: CS1001): ".
    05 LINE 3 COLUMN 28 PIC X(6) USING SYL-COURSE-ID.
```

This demonstrates:
- Terminal screen clearing with BLANK SCREEN
- Absolute positioning using LINE and COLUMN clauses
- Field definition with input/output capabilities
- Form-based data entry system

### 2. File Operations and Persistence

The application showcases indexed file handling for data persistence:

```cobol
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SYLLABUS-FILE
        ASSIGN TO "syllabus.dat"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS SYL-COURSE-ID
        FILE STATUS IS WS-FILE-STATUS.
```

Key features:
- INDEXED organization (similar to VSAM files in mainframe systems)
- DYNAMIC access mode for both sequential and random access
- Primary key definition (SYL-COURSE-ID)
- Error handling using FILE STATUS

### 3. Complex Data Structures

The system uses copybooks for data structure definitions:

- **SYLFILE.cpy**: Defines the syllabus record structure with course information and weekly plans
- **DEPFILE.cpy**: Defines the department record structure
- **TEAFILE.cpy**: Defines the teacher record structure

These structures likely include:
- Nested hierarchy with group items
- OCCURS clauses for table handling (seen in weekly plan implementation)
- Reusable definitions across multiple programs

### 4. Control Flow and Program Logic

The application demonstrates sophisticated COBOL control structures:

```cobol
EVALUATE WS-USER-CHOICE
    WHEN 1
        PERFORM CALL-SYLLABUS-REGISTER
    WHEN 2
        PERFORM CALL-SYLLABUS-UPDATE
    WHEN 3
        PERFORM CALL-SYLLABUS-DELETE
    WHEN 4
        PERFORM CALL-SYLLABUS-QUERY
    WHEN 5
        PERFORM CALL-SYLLABUS-LIST
    WHEN 6
        PERFORM CALL-REPORT-GENERATE
    WHEN 9
        MOVE 1 TO WS-EXIT-FLAG
    WHEN OTHER
        DISPLAY "無効な選択です。再試行してください。"
END-EVALUATE.
```

Key programming patterns:
- EVALUATE statements for multi-way branching (similar to switch/case)
- PERFORM statements for procedure calls
- Conditional looping with PERFORM UNTIL
- Section and paragraph organization

### 5. Subprogram Communication

The system demonstrates inter-program communication:

```cobol
CALL "SYLCOM" USING WS-FUNCTION-CODE, WS-PARAM-1,
               WS-PARAM-2, WS-RESULT, WS-RETURN-CODE.
```

Features:
- Static subprogram calls
- Parameter passing by reference
- Return code handling
- Function code-based operation selection

### 6. Error Handling Mechanism

Robust error handling is implemented throughout:

```cobol
01 WS-FILE-STATUS           PIC XX VALUE "00".
   88 WS-FILE-SUCCESS       VALUE "00".
   88 WS-FILE-DUP           VALUE "22".
   88 WS-FILE-NOT-FOUND     VALUE "23".
```

Key techniques:
- Level-88 condition names for readable status checks
- Explicit error message display
- Exception handling in file operations
- Status code propagation

## CRUD Implementation

The system provides a complete CRUD (Create, Read, Update, Delete) implementation:

1. **Create**: SYLREG.cbl - Registration of new syllabus entries
   - Data entry screens for all fields
   - Multi-screen input for large datasets
   - Validation before writing records

2. **Read**: SYLQRY.cbl - Detailed query of syllabus information
   - Key-based record retrieval
   - Multi-screen display of comprehensive information
   - Navigation through complex data

3. **Update**: SYLUPD.cbl - Modification of existing syllabus entries
   - Field-by-field updates
   - Current data display before modification
   - Complete or selective updates

4. **Delete**: SYLDEL.cbl - Removal of syllabus entries
   - Record selection by key
   - Confirmation before deletion
   - Status reporting after operation

5. **List**: SYLLST.cbl - Filtered listing of syllabus entries
   - Multiple filtering options
   - Paginated display
   - Summary information

6. **Report**: SYLRPT.cbl - Formatted reporting
   - Multiple report types
   - Filtering and selection criteria
   - Formatted output for printing

## Business Domain

The application models an educational institution's syllabus management system with these key entities:

1. **Courses/Syllabi**: Core educational offerings with:
   - Identification (course codes)
   - Descriptive information (names, descriptions)
   - Scheduling information (semester, credits)
   - Weekly teaching plans

2. **Departments**: Academic organizational units

3. **Teachers**: Faculty members associated with courses

This domain model demonstrates how COBOL applications typically represent business entities and relationships through file structures and program logic rather than through object-oriented models.

## Conclusion

The Syllabus Management System exemplifies classic COBOL application architecture with these characteristics:

1. Modular architecture with specialized programs
2. Screen Section for text-based user interfaces
3. Indexed file operations for data persistence
4. Complex data structures using copybooks
5. Procedural programming paradigm with structured control flow
6. Subprogram calling mechanisms
7. Comprehensive error handling
8. Complete CRUD implementation

This sample effectively demonstrates the patterns and techniques found in many legacy COBOL applications still running critical business operations in various industries today, particularly in education, finance, insurance, and government sectors.

## References

## Licence

Released under the [MIT license](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)

## Author

- github: <https://github.com/shinyay>
- twitter: <https://twitter.com/yanashin18618>
- mastodon: <https://mastodon.social/@yanashin>
