# MosaBiomeUtils

Standardized Project Structure and Workflow for MosaBiome Statistical Analysis

## Overview

MosaBiomeUtils is an R package that provides a comprehensive toolkit for standardizing project structure, workflow, and reporting for statistical analyses. It ensures consistency across projects, simplifies report generation using Quarto, and promotes reproducible research practices.

## Features

- **Project Templates**: Create standardized folder structures with a single command
- **Quarto Integration**: Generate analysis reports with consistent formatting
- **Separated Outputs**: Keep source files (.qmd) separate from rendered outputs
- **Author Management**: Persistent author name configuration across sessions
- **Code Formatting**: Custom styler rules for consistent code style
- **IDE Addins**: Quick access to common tasks in RStudio/Positron

## Installation

```r
# Install from GitHub
# devtools::install_github("MosaBiome/MosaBiomeUtils")

# Or install locally
devtools::install("path/to/MosaBiomeUtils")
```

### Dependencies

**Required:**
- `fs` - File system operations
- `here` - Project-relative paths
- `yaml` - YAML file handling
- `rstudioapi` - IDE integration
- `quarto` - Document rendering

**Optional:**
- `styler` - Code formatting (for `MB_style_report()`)

**System Requirements:**
- [Quarto CLI](https://quarto.org/docs/get-started/) must be installed
- For PDF output: LaTeX distribution (e.g., TinyTeX)

## Quick Start

### 1. Create a New Project

```r
library(MosaBiomeUtils)

MBU_create_new_project(
  path = "~/projects/my_analysis",
  project_id = "PRJ-2024-001",
  project_name = "My Statistical Analysis"
)
```

This creates:
```
my_analysis/
├── 1.data/
│   └── 1.1.data_analysis_ready/
├── 2.analysis/
├── 3.supporting_code/
├── 4.results_files/
├── project_config.yml
├── my_analysis.Rproj
└── readme.txt
```

### 2. Create a New Analysis

```r
# Navigate to your project first
setwd("~/projects/my_analysis")

# Create a new analysis file
MBU_new_analysis("descriptive_statistics")
```

This creates:
- `2.analysis/descriptive_statistics.qmd` - Your Quarto document
- `4.results_files/descriptive_statistics/` - Output directory with subdirectories

### 3. Render Your Report

```r
# With the .qmd file open in Positron/RStudio:
MBU_render_report()

# Or specify a file directly:
MBU_render_report("2.analysis/descriptive_statistics.qmd")
```

Output is saved to: `4.results_files/descriptive_statistics/descriptive_statistics.html`

## Project Structure

MosaBiomeUtils enforces a consistent folder structure:

| Folder | Purpose |
|--------|---------|
| `1.data/` | Raw data files (never modify) |
| `1.data/1.1.data_analysis_ready/` | Cleaned data ready for analysis |
| `2.analysis/` | Quarto (.qmd) analysis files |
| `3.supporting_code/` | Helper R scripts and utilities |
| `4.results_files/` | Rendered reports and outputs |
| `4.results_files/<name>/4.1.plots/` | Plot images |
| `4.results_files/<name>/4.2.tables/` | Table outputs |
| `4.results_files/<name>/4.3.data_objects/` | R data objects (.rds) |

## Configuration

### project_config.yml

Each project has a configuration file:

```yaml
project_id: "PRJ-2024-001"
project_name: "My Statistical Analysis"
output_dir: "4.results_files"
```

### Author Name

Set your author name (saved to `~/.Rprofile`):

```r
set_author_name()
# Enter your full name (e.g., John Doe): Dr. Jane Smith

# Or set manually:
options(MosaBiomeUtils.author = "Dr. Jane Smith")
```

## Functions Reference

### Project Management

| Function | Description |
|----------|-------------|
| `MBU_create_new_project()` | Create a new project with standard structure |
| `MBU_new_analysis()` | Create a new Quarto analysis file |
| `MBU_render_report()` | Render Quarto document to output directory |

### Configuration

| Function | Description |
|----------|-------------|
| `set_author_name()` | Set or update author name for templates |

### Code Formatting

| Function | Description |
|----------|-------------|
| `MB_style_report()` | Apply MosaBiome code style (addin) |
| `report_custom_transformers()` | Get custom styler transformers |

### Text Manipulation

| Function | Description |
|----------|-------------|
| `wrapInQuote()` | Wrap vector elements in quotes (addin) |
| `underlineHtml()` | Add HTML underline tags (addin) |

## IDE Addins

MosaBiomeUtils provides RStudio/Positron addins accessible via **Addins** menu:

| Addin | Shortcut Suggestion | Description |
|-------|---------------------|-------------|
| Render Quarto Report | `Ctrl+Shift+R` | Render current .qmd file |
| MosaBiome Style Report | `Ctrl+Shift+S` | Format code with custom style |
| Wrap in quotes | `Ctrl+Shift+Q` | Quote vector elements |
| Underline for HTML | `Ctrl+Shift+U` | Add HTML underline tags |

### Setting Up Keyboard Shortcuts

1. Go to **Tools > Modify Keyboard Shortcuts**
2. Search for the addin name
3. Click on the shortcut field and press your desired key combination

## Workflow Example

```r
library(MosaBiomeUtils)

# 1. Create project (once)
MBU_create_new_project(
  path = "~/projects/microbiome_study",
  project_id = "MB-2024-001",
  project_name = "Gut Microbiome Analysis"
)

# 2. Open the project
# (Open microbiome_study.Rproj in Positron/RStudio)

# 3. Create analyses
MBU_new_analysis("01_data_preparation")
MBU_new_analysis("02_alpha_diversity")
MBU_new_analysis("03_beta_diversity")
MBU_new_analysis("04_differential_abundance")

# 4. Edit each .qmd file in 2.analysis/

# 5. Render reports
MBU_render_report("2.analysis/01_data_preparation.qmd")
MBU_render_report("2.analysis/02_alpha_diversity.qmd")
# ... or just call MBU_render_report() with the file open

# 6. Find outputs in 4.results_files/<analysis_name>/
```

## Quarto Template

The generated `.qmd` files include:

- Project metadata (ID, name, author)
- HTML output with:
  - Table of contents
  - Code folding
  - Dark/light theme toggle
  - Interactive features
- Starter sections for structured reporting
- Automatic session info capture

## Troubleshooting

### "project_config.yml not found"

You're not in a MosaBiome project directory. Either:
- Navigate to your project: `setwd("~/projects/my_project")`
- Open the `.Rproj` file
- Create a new project: `MBU_create_new_project()`

### "Could not detect current file"

When using `MBU_render_report()` without arguments:
- Make sure a `.qmd` file is open and active
- Save the file before rendering
- Or specify the file path explicitly

### "Quarto rendering failed"

- Ensure Quarto CLI is installed: https://quarto.org/docs/get-started/
- Check your `.qmd` file for syntax errors
- For PDF: Install TinyTeX with `quarto install tinytex`

### Addins not showing

- Restart R session after installing the package
- Check that `rstudioapi` package is installed

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run `devtools::check()` to ensure no issues
5. Submit a pull request

## License

MIT License - See [LICENSE](LICENSE) for details.

## Authors

MosaBiome Team

---

*Built with MosaBiomeUtils - Standardizing statistical analysis workflows*
