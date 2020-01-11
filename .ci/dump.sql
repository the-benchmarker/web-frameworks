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
-- Name: ar_internal_metadata; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.ar_internal_metadata (
    key character varying NOT NULL,
    value character varying,
    created_at timestamp(6) without time zone NOT NULL,
    updated_at timestamp(6) without time zone NOT NULL
);


ALTER TABLE public.ar_internal_metadata OWNER TO postgres;

--
-- Name: computable; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.computable (
    id bigint NOT NULL,
    writable_id bigint,
    engine_id bigint,
    metric_id bigint
);


ALTER TABLE public.computable OWNER TO postgres;

--
-- Name: computable_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.computable_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.computable_id_seq OWNER TO postgres;

--
-- Name: computable_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.computable_id_seq OWNED BY public.computable.id;


--
-- Name: engines; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.engines (
    id bigint NOT NULL,
    label character varying
);


ALTER TABLE public.engines OWNER TO postgres;

--
-- Name: engines_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.engines_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.engines_id_seq OWNER TO postgres;

--
-- Name: engines_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.engines_id_seq OWNED BY public.engines.id;


--
-- Name: frameworks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.frameworks (
    id bigint NOT NULL,
    label character varying
);


ALTER TABLE public.frameworks OWNER TO postgres;

--
-- Name: frameworks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.frameworks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.frameworks_id_seq OWNER TO postgres;

--
-- Name: frameworks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.frameworks_id_seq OWNED BY public.frameworks.id;


--
-- Name: languages; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.languages (
    id bigint NOT NULL,
    label character varying
);


ALTER TABLE public.languages OWNER TO postgres;

--
-- Name: languages_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.languages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.languages_id_seq OWNER TO postgres;

--
-- Name: languages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.languages_id_seq OWNED BY public.languages.id;


--
-- Name: metrics; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.metrics (
    id bigint NOT NULL,
    label character varying
);


ALTER TABLE public.metrics OWNER TO postgres;

--
-- Name: metrics_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.metrics_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.metrics_id_seq OWNER TO postgres;

--
-- Name: metrics_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.metrics_id_seq OWNED BY public.metrics.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version character varying NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: values; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."values" (
    id bigint NOT NULL,
    computable_id bigint,
    value double precision
);


ALTER TABLE public."values" OWNER TO postgres;

--
-- Name: values_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.values_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.values_id_seq OWNER TO postgres;

--
-- Name: values_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.values_id_seq OWNED BY public."values".id;


--
-- Name: writable; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.writable (
    id bigint NOT NULL,
    framework_id bigint,
    language_id bigint
);


ALTER TABLE public.writable OWNER TO postgres;

--
-- Name: writable_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.writable_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.writable_id_seq OWNER TO postgres;

--
-- Name: writable_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.writable_id_seq OWNED BY public.writable.id;


--
-- Name: computable id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.computable ALTER COLUMN id SET DEFAULT nextval('public.computable_id_seq'::regclass);


--
-- Name: engines id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.engines ALTER COLUMN id SET DEFAULT nextval('public.engines_id_seq'::regclass);


--
-- Name: frameworks id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.frameworks ALTER COLUMN id SET DEFAULT nextval('public.frameworks_id_seq'::regclass);


--
-- Name: languages id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.languages ALTER COLUMN id SET DEFAULT nextval('public.languages_id_seq'::regclass);


--
-- Name: metrics id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.metrics ALTER COLUMN id SET DEFAULT nextval('public.metrics_id_seq'::regclass);


--
-- Name: values id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."values" ALTER COLUMN id SET DEFAULT nextval('public.values_id_seq'::regclass);


--
-- Name: writable id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.writable ALTER COLUMN id SET DEFAULT nextval('public.writable_id_seq'::regclass);


--
-- Data for Name: ar_internal_metadata; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.ar_internal_metadata (key, value, created_at, updated_at) FROM stdin;
environment	default_env	2020-01-11 15:39:46.372581	2020-01-11 15:39:46.372581
\.


--
-- Data for Name: computable; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.computable (id, writable_id, engine_id, metric_id) FROM stdin;
1	1	1	1
2	1	1	2
3	1	1	3
4	1	1	4
5	1	1	5
6	1	1	6
7	1	1	7
8	1	1	8
9	1	1	9
10	1	1	10
11	1	1	11
12	1	1	12
13	1	1	13
14	1	1	14
15	1	1	15
16	1	1	16
17	1	1	17
\.


--
-- Data for Name: engines; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.engines (id, label) FROM stdin;
1	puma
\.


--
-- Data for Name: frameworks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.frameworks (id, label) FROM stdin;
1	rails
\.


--
-- Data for Name: languages; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.languages (id, label) FROM stdin;
1	ruby
\.


--
-- Data for Name: metrics; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.metrics (id, label) FROM stdin;
1	request_duration
2	request_total
3	request_per_second
4	request_bytes
5	error_socket
6	error_read
7	error_write
8	error_http
9	error_timeout
10	latency_minimum
11	latency_maximum
12	latency_average
13	latency_deviation
14	percentile_fifty
15	percentile_ninety
16	percentile_ninety_nine
17	percentile_ninety_nine_ninety
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version) FROM stdin;
20191014111447
\.


--
-- Data for Name: values; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."values" (id, computable_id, value) FROM stdin;
1	1	15051350
2	2	60907
3	3	4046
4	4	23692823
5	5	0
6	6	0
7	7	0
8	8	0
9	9	2
10	10	923
11	11	866290
12	12	46909
13	13	82094
14	14	3558
15	15	58568
16	16	153922
17	17	367022
\.


--
-- Data for Name: writable; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.writable (id, framework_id, language_id) FROM stdin;
1	1	1
\.


--
-- Name: computable_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.computable_id_seq', 17, true);


--
-- Name: engines_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.engines_id_seq', 1, true);


--
-- Name: frameworks_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.frameworks_id_seq', 1, true);


--
-- Name: languages_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.languages_id_seq', 2, true);


--
-- Name: metrics_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.metrics_id_seq', 17, true);


--
-- Name: values_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.values_id_seq', 17, true);


--
-- Name: writable_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.writable_id_seq', 1, true);


--
-- Name: ar_internal_metadata ar_internal_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ar_internal_metadata
    ADD CONSTRAINT ar_internal_metadata_pkey PRIMARY KEY (key);


--
-- Name: computable computable_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.computable
    ADD CONSTRAINT computable_pkey PRIMARY KEY (id);


--
-- Name: engines engines_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.engines
    ADD CONSTRAINT engines_pkey PRIMARY KEY (id);


--
-- Name: frameworks frameworks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.frameworks
    ADD CONSTRAINT frameworks_pkey PRIMARY KEY (id);


--
-- Name: languages languages_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.languages
    ADD CONSTRAINT languages_pkey PRIMARY KEY (id);


--
-- Name: metrics metrics_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.metrics
    ADD CONSTRAINT metrics_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: values values_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."values"
    ADD CONSTRAINT values_pkey PRIMARY KEY (id);


--
-- Name: writable writable_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.writable
    ADD CONSTRAINT writable_pkey PRIMARY KEY (id);


--
-- Name: index_computable_on_engine_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_computable_on_engine_id ON public.computable USING btree (engine_id);


--
-- Name: index_computable_on_metric_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_computable_on_metric_id ON public.computable USING btree (metric_id);


--
-- Name: index_computable_on_writable_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_computable_on_writable_id ON public.computable USING btree (writable_id);


--
-- Name: index_engines_on_label; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_engines_on_label ON public.engines USING btree (label);


--
-- Name: index_frameworks_on_label; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_frameworks_on_label ON public.frameworks USING btree (label);


--
-- Name: index_languages_on_label; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_languages_on_label ON public.languages USING btree (label);


--
-- Name: index_metrics_on_label; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_metrics_on_label ON public.metrics USING btree (label);


--
-- Name: index_values_on_computable_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_values_on_computable_id ON public."values" USING btree (computable_id);


--
-- Name: index_writable_on_framework_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_writable_on_framework_id ON public.writable USING btree (framework_id);


--
-- Name: index_writable_on_framework_id_and_language_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_writable_on_framework_id_and_language_id ON public.writable USING btree (framework_id, language_id);


--
-- Name: index_writable_on_language_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_writable_on_language_id ON public.writable USING btree (language_id);


--
-- PostgreSQL database dump complete
--

