import openai
import json

# Replace with your actual API key
openai.api_key = "Your OpenAI KEY"

# Load the test cases from the JSON file
with open('val_lr.json') as f:
    test_cases = json.load(f)

def test_model(test_cases):
    passed_cases = 0
    total_cases = len(test_cases)

    for i, test in enumerate(test_cases):
        prompt = f"Context: {test['context']}\nQuestion: {test['question']}\nChoices: {', '.join(test['answers'])}\nAnswer with the correct choice:"
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo-1106",
            messages=[
                {"role": "user", "content": prompt}
            ],
            temperature=0,  # Set temperature to 0 for more deterministic output
            max_tokens=256,
            top_p=1,  # Set top_p to 1 to use all probabilities
            frequency_penalty=0,
            presence_penalty=0
        )
        
        # Extract the response text
        response_text = response['choices'][0]['message']['content'].strip()

        # Check if the response matches the expected response
        expected_response = test['answers'][test['label']]
        if response_text == expected_response:
            passed_cases += 1
            print(f"Test case {i + 1} passed.")
        #else:
            #print(f"Test case {i + 1} failed.")
            #print(f"Prompt: {prompt}")
            #print(f"Expected: {expected_response}")
            #print(f"Got: {response_text}")
            #print()

    # Calculate and print the accuracy
    accuracy = (passed_cases / total_cases) * 100
    print(f"Accuracy: {accuracy:.2f}%")

# Run the test cases
test_model(test_cases)

#Accuracy Rate for gpt-3.5-turbo-1106: 39.53%
