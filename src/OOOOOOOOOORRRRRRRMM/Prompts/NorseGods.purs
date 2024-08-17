module OOOOOOOOOORRRRRRRMM.Prompts.NorseGods where


norseGods = """
CREATE TABLE favorite_ancient_norse_gods (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    domain VARCHAR(100),
    worshipers_count INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE anthropologists_researching_ancient_norse_gods (
    id SERIAL PRIMARY KEY,
    full_name VARCHAR(255) NOT NULL,
    specialty VARCHAR(100),
    favorite_god_id INTEGER REFERENCES favorite_ancient_norse_gods(id) ON DELETE SET NULL,
    research_start_date DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE airlines_friendly_to_anthropologists (
    id SERIAL PRIMARY KEY,
    airline_name VARCHAR(255) NOT NULL,
    discount_percentage DECIMAL(5, 2),
    anthropologist_id INTEGER REFERENCES anthropologists_researching_ancient_norse_gods(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE obscure_norse_god_research_papers (
    id SERIAL PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    publication_year INTEGER,
    anthropologist_id INTEGER REFERENCES anthropologists_researching_ancient_norse_gods(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
""" :: String