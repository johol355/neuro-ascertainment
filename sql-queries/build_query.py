def read_file(path):
    """
    Reads the content of a file at the given path.

    Parameters:
        path (str): The file path to read.

    Returns:
        str: The content of the file.

    Raises:
        FileNotFoundError: If the file does not exist.
        IOError: If there is an error reading the file.
    """
    if not isinstance(path, str):
        raise ValueError("The path must be a string.")
    
    try:
        with open(path, "r") as file:
            return file.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"File not found: {path}")
    except IOError as e:
        raise IOError(f"Error reading file {path}: {e}")

def build_query(paths, final_query):
    """
    Combines the content of multiple files and appends a final query.

    Parameters:
        paths (list of str): A list of file paths to read.
        final_query (str): A SQL query string to append at the end.

    Returns:
        str: The combined file contents and final query.

    Raises:
        ValueError: If paths is not a list or contains non-string elements.
        ValueError: If final_query is not a string.
    """
    if not isinstance(paths, list):
        raise ValueError("The paths parameter must be a list.")
    if not all(isinstance(path, str) for path in paths):
        raise ValueError("All elements in paths must be strings.")
    if not isinstance(final_query, str):
        raise ValueError("The final_query parameter must be a string.")
    
    try:
        contents = ",\n\n".join(read_file(path) for path in paths)
        return f"{contents}\n\n{final_query}"
    except Exception as e:
        raise RuntimeError(f"Failed to build query: {e}")