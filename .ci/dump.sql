--
-- PostgreSQL database dump
--

-- Dumped from database version 11.5
-- Dumped by pg_dump version 11.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: ar_internal_metadata; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ar_internal_metadata (
    key character varying NOT NULL,
    value character varying,
    created_at timestamp(6) without time zone NOT NULL,
    updated_at timestamp(6) without time zone NOT NULL
);


--
-- Name: frameworks; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.frameworks (
    id bigint NOT NULL,
    language_id bigint,
    label character varying
);


--
-- Name: frameworks_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.frameworks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: frameworks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.frameworks_id_seq OWNED BY public.frameworks.id;


--
-- Name: keys; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.keys (
    id bigint NOT NULL,
    label character varying
);


--
-- Name: keys_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.keys_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: keys_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.keys_id_seq OWNED BY public.keys.id;


--
-- Name: languages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.languages (
    id bigint NOT NULL,
    label character varying
);


--
-- Name: languages_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.languages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: languages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.languages_id_seq OWNED BY public.languages.id;


--
-- Name: metrics; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.metrics (
    framework_id bigint,
    value_id bigint
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying NOT NULL
);


--
-- Name: values; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public."values" (
    id bigint NOT NULL,
    value double precision,
    key_id bigint
);


--
-- Name: values_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.values_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: values_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.values_id_seq OWNED BY public."values".id;


--
-- Name: writable; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.writable (
    language_id bigint,
    framework_id bigint
);


--
-- Name: frameworks id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.frameworks ALTER COLUMN id SET DEFAULT nextval('public.frameworks_id_seq'::regclass);


--
-- Name: keys id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.keys ALTER COLUMN id SET DEFAULT nextval('public.keys_id_seq'::regclass);


--
-- Name: languages id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.languages ALTER COLUMN id SET DEFAULT nextval('public.languages_id_seq'::regclass);


--
-- Name: values id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public."values" ALTER COLUMN id SET DEFAULT nextval('public.values_id_seq'::regclass);


--
-- Name: ar_internal_metadata ar_internal_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ar_internal_metadata
    ADD CONSTRAINT ar_internal_metadata_pkey PRIMARY KEY (key);


--
-- Name: frameworks frameworks_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.frameworks
    ADD CONSTRAINT frameworks_pkey PRIMARY KEY (id);


--
-- Name: keys keys_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.keys
    ADD CONSTRAINT keys_pkey PRIMARY KEY (id);


--
-- Name: languages languages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.languages
    ADD CONSTRAINT languages_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: values values_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public."values"
    ADD CONSTRAINT values_pkey PRIMARY KEY (id);


--
-- Name: index_frameworks_on_language_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_frameworks_on_language_id ON public.frameworks USING btree (language_id);


--
-- Name: index_frameworks_on_language_id_and_label; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX index_frameworks_on_language_id_and_label ON public.frameworks USING btree (language_id, label);


--
-- Name: index_keys_on_label; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX index_keys_on_label ON public.keys USING btree (label);


--
-- Name: index_languages_on_label; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX index_languages_on_label ON public.languages USING btree (label);


--
-- Name: index_metrics_on_framework_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_metrics_on_framework_id ON public.metrics USING btree (framework_id);


--
-- Name: index_metrics_on_value_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_metrics_on_value_id ON public.metrics USING btree (value_id);


--
-- Name: index_values_on_key_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_values_on_key_id ON public."values" USING btree (key_id);


--
-- Name: index_writable_on_framework_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_writable_on_framework_id ON public.writable USING btree (framework_id);


--
-- Name: index_writable_on_language_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_writable_on_language_id ON public.writable USING btree (language_id);


--
-- PostgreSQL database dump complete
--

