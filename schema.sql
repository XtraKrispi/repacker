-- WARNING: This schema is for context only and is not meant to be run.
-- Table order and constraints may not be valid for execution.

CREATE TABLE public.profiles (
  id bigint GENERATED ALWAYS AS IDENTITY NOT NULL,
  user_id uuid NOT NULL DEFAULT auth.uid
() UNIQUE,
  first_name character varying NOT NULL,
  last_name character varying NOT NULL,
  created_at timestamp
with time zone NOT NULL DEFAULT now
(),
  username character varying NOT NULL DEFAULT ''::character varying,
  email character varying NOT NULL,
  CONSTRAINT profiles_pkey PRIMARY KEY
(id),
  CONSTRAINT profiles_user_id_fkey FOREIGN KEY
(user_id) REFERENCES auth.users
(id)
);
CREATE TABLE public.instructions (
  id bigint GENERATED ALWAYS AS IDENTITY NOT NULL,
  bgg_id character varying NOT NULL,
  created_by uuid NOT NULL DEFAULT auth.uid
(),
  created_at timestamp
with time zone NOT NULL DEFAULT now
(),
  instructions_key uuid NOT NULL UNIQUE,
  data jsonb NOT NULL,
  CONSTRAINT instructions_pkey PRIMARY KEY
(id),
  CONSTRAINT instructions_created_by_fkey FOREIGN KEY
(created_by) REFERENCES auth.users
(id)
);