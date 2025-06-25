# uid_np
Code to extract NPs from the RSC and calculate surprisal-based complexity measures

To run the code:

* create a virtual environment, install numpy (it should be the only dependency)
* in terminal write:

```bash
python np_pipeline.py <your_input_folder> <your_output_folder/csv_file>
```
input and output folders should already exist before running the pipeline.

TODO: 

- [ ] write core function `identify_NPs_in_sentence` (Isa)
- [ ] clean up paths and use pathlib for better path handling and folder creation (Ari)
- [ ] use argparse instead of sys for better cli (Ari)
- [ ] set up requirements / package the thing (Ari)
- [ ] check fluctuation cpx in light of Paolo's corrections (Ari)

