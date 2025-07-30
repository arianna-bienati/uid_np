# uid_np
Code to extract NPs from the RSC and calculate surprisal-based complexity measures

To run the code:

* create a virtual environment, install numpy (it should be the only dependency)
* in terminal write:

```bash
python get_NP_data.py <your_input_folder> <your_output_folder/csv_file>
```
input and output folders should already exist before running the pipeline.

TODO: 

- [x] write core function `identify_NPs_in_sentence` (Isa) -> see get_NP_data.py for a full implementation
- [x] clean up paths and use pathlib for better path handling and folder creation (Ari)
- [x] use argparse instead of sys for better cli (Ari)
- [ ] set up requirements / package the thing (Ari)
- [x] check fluctuation cpx in light of Paolo's corrections (Ari)

## Decisions

20250730 meeting:
* Model(s) specification: UID_dev ~ year_centered + NP_length + head_synt_role + (1|author) + (1|head_lemma)
* UID_dev and IFC are calculated only for NPs with number of constituents >= 3. Since UID_dev and IFC are based on the difference between information contents of preceding and following tokens, it is necessary at least to have two transitions in order to compute UID_dev or IFC meaningfully.
* To keep the model parsimonious, we concentrate only on Series A journals.
* Threshold for lemma frequency: 5.
* Coordination: TBD (cases like "Isabell and Arianna have submitted an abstract and a presentation" are for now excluded and only the first NP gets extracted (Isabell; an abstract); coordination remains in relative clauses such as "Isabell and Arianna who have submitted an abstract and a presentation will go to Siena" --> (Isabell who have submitted an abstract and a presentation)).

Logical next steps:
- [ ] write some tests to be sure about the functionality of the NPs extractor and its adherence to definition of NPs.
- [ ] prepare slides
- [ ] run statistical analysis