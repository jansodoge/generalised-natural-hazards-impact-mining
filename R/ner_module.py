from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline
import spacy
import pandas

def process_ner(test):
    nlp = spacy.load("de_core_news_lg")

    def extract_tokens_plus_meta(doc: spacy.tokens.doc.Doc):
        """Extract tokens and metadata from individual spaCy doc."""
        return [
            (i.text, i.i, i.lemma_, i.ent_type_, i.tag_,
             i.dep_, i.pos_, i.is_stop, i.is_alpha,
             i.is_digit, i.is_punct) for i in doc
        ]

    def tidy_tokens(docs, ids):
        """Extract tokens and metadata from list of spaCy docs."""

        cols = [
            "doc_id", "token", "token_order", "lemma",
            "ent_type", "tag", "dep", "pos", "is_stop",
            "is_alpha", "is_digit", "is_punct"
        ]

        meta_df = []

        for doc_id, doc in zip(ids, docs):  # Fixed variable name to 'doc_id'
            meta = extract_tokens_plus_meta(doc)
            meta = pandas.DataFrame(meta)
            meta.columns = cols[1:]
            meta['doc_id'] = doc_id  # Attach the doc_id to each row
            meta_df.append(meta)

        return pandas.concat(meta_df)

    annotations = pandas.DataFrame(tidy_tokens(docs=nlp.pipe(test.text), ids=test.ID))
    annotations = annotations.loc[annotations['ent_type'] == "LOC"]

    return [annotations]
